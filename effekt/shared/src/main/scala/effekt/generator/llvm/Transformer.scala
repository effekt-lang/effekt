package effekt
package generator
package llvm

import effekt.machine
import effekt.util.intercalate
import effekt.util.messages.ErrorReporter
import effekt.machine.analysis.*

import scala.collection.mutable

object Transformer {

  val llvmFeatureFlags: List[String] = List("llvm")

  val escapeSeqs: Map[Char, String] = Map('\'' -> raw"'", '\"' -> raw"\"", '\\' -> raw"\\", '\n' -> raw"\n", '\t' -> raw"\t", '\r' -> raw"\r")

  def transform(program: machine.Program)(using ErrorReporter): List[Definition] = program match {
    case machine.Program(declarations, statement) =>
      emit(Comment("program"))

      given MC: ModuleContext = ModuleContext();
      given FC: FunctionContext = FunctionContext();
      given BC: BlockContext = BlockContext();

      // TODO proper initialization of runtime
      emit(Call("stackPointer", stackPointerType, malloc, List(ConstantInt(256 * 1024 * 1024))));
      emit(Store(ConstantGlobal(PointerType(), "base"), LocalReference(stackPointerType, "stackPointer")));
      pushReturnAddress("topLevel", "topLevelSharer", "topLevelEraser");

      val terminator = transform(statement);

      val definitions = MC.definitions; MC.definitions = null;
      val basicBlocks = FC.basicBlocks; FC.basicBlocks = null;
      val instructions = BC.instructions; BC.instructions = null;

      val entryBlock = BasicBlock("entry", instructions, terminator)
      val entryFunction = Function(VoidType(), "effektMain", List(), entryBlock :: basicBlocks)
      declarations.map(transform) ++ definitions :+ entryFunction
  }

  // context getters
  private def MC(using MC: ModuleContext): ModuleContext = MC
  private def FC(using FC: FunctionContext): FunctionContext = FC
  private def BC(using BC: BlockContext): BlockContext = BC

  def transform(declaration: machine.Declaration)(using ErrorReporter): Definition =
    declaration match {
      case machine.Extern(functionName, parameters, returnType, body ) =>
        body match {
          case machine.ExternBody.StringExternBody(_, contents) =>
            VerbatimFunction(transform(returnType), functionName, parameters.map {
              case machine.Variable(name, tpe) => Parameter(transform(tpe), name)
            }, "; declaration extern\n    " ++ transform(contents))
          case u: machine.ExternBody.Unsupported =>
            u.report
            VerbatimFunction(transform(returnType), functionName, parameters.map {
                case machine.Variable(name, tpe) => Parameter(transform(tpe), name)
              },
              """call void @hole()
                |unreachable
                |""".stripMargin)
        }
      case machine.Include(ff, content) =>
        Verbatim("; declaration include" ++ content)
    }

  def transform(t: Template[machine.Variable]): String = "; variable\n    " ++ intercalate(t.strings, t.args.map {
    case machine.Variable(name, tpe) => PrettyPrinter.localName(name)
  }).mkString

  def transform(statement: machine.Statement)(using ModuleContext, FunctionContext, BlockContext): Terminator =
    statement match {

      case machine.Def(machine.Label(name, environment), body, rest) =>
        val parameters = environment.map { case machine.Variable(name, tpe) => Parameter(transform(tpe), name) }
        defineFunction(name, parameters :+ Parameter(stackPointerType, "stackPointer")) {
          emit(Comment("statement definition"))
          eraseValues(environment, freeVariables(body))
          transform(body)
        }

        transform(rest)

      case machine.Jump(label) =>
        emit(Comment("statement jump"))
        shareValues(label.environment, Set())

        val arguments = label.environment.map(transform)
        emit(TailCall(transform(label), arguments :+ getStackPointer()))
        RetVoid()

      case machine.Substitute(bindings, rest) =>
        emit(Comment("statement substitution"))
        withBindings(bindings) { () =>
          transform(rest)
        }

      case machine.Construct(variable, tag, values, rest) =>
        emit(Comment("statement construct"))
        val obj = produceObject(values, freeVariables(rest))
        val tmpName = freshName("tmp")
        emit(InsertValue(tmpName, ConstantAggregateZero(positiveType), ConstantInt(tag), 0))
        emit(InsertValue(variable.name, LocalReference(positiveType, tmpName), obj, 1))

        eraseValues(List(variable), freeVariables(rest))
        transform(rest)

      case machine.Switch(value, clauses, default) =>
        emit(Comment("statement switch"))
        shareValues(List(value), clauses.flatMap(freeVariables).toSet)

        val tagName = freshName("tag")
        val objName = freshName("obj")
        emit(ExtractValue(tagName, transform(value), 0))
        emit(ExtractValue(objName, transform(value), 1))

        val freeInClauses = clauses.flatMap(freeVariables)

        val stackPointer = getStackPointer();
        def labelClause(clause: machine.Clause): String = {
          implicit val BC = BlockContext()
          BC.stackPointer = stackPointer

          consumeObject(LocalReference(objectType, objName), clause.parameters, freeVariables(clause.body));
          eraseValues(freeInClauses, freeVariables(clause));

          val terminator = transform(clause.body);

          val instructions = BC.instructions;
          BC.instructions = null;

          val label = freshName("label");
          emit(BasicBlock(label, instructions, terminator))
          label
        }

        val defaultLabel = default match {
          case Some(clause) => labelClause(clause)
          case None =>
            val label = freshName("label");
            emit(BasicBlock(label, List(), RetVoid()))
            label
        }

        val labels = clauses.map {
          case (tag, clause) => (tag, labelClause(clause))
        }

        Switch(LocalReference(IntegerType64(), tagName), defaultLabel, labels)

        // this
      case machine.New(variable, clauses, rest) =>
        emit(Comment("statement new"))
        val closureEnvironment = freeVariables(clauses).toList;

        val clauseNames = clauses.map { clause =>
          val clauseName = freshName(variable.name);
          defineFunction(clauseName, List(Parameter(objectType, "obj"), Parameter(environmentType, "environment"), Parameter(stackPointerType, "stackPointer"))) {
            emit(Comment("statement new"))
            consumeObject(LocalReference(objectType, "obj"), closureEnvironment, freeVariables(clause));
            loadEnvironment(initialEnvironmentPointer, clause.parameters);
            eraseValues(clause.parameters, freeVariables(clause.body));
            transform(clause.body);
          }
          ConstantGlobal(methodType, clauseName)
        }

        val arrayName = freshName(variable.name)
        emit(GlobalConstant(arrayName, ConstantArray(methodType, clauseNames)))

        val obj = produceObject(closureEnvironment, freeVariables(rest));
        val tmpName = freshName("tmp");
        emit(InsertValue(tmpName, ConstantAggregateZero(negativeType), ConstantGlobal(PointerType(), arrayName), 0));
        emit(InsertValue(variable.name, LocalReference(negativeType, tmpName), obj, 1));

        eraseValues(List(variable), freeVariables(rest));
        transform(rest)

        // this
      case machine.Invoke(value, tag, values) =>
        emit(Comment("statement invoke"))
        shareValues(value :: values, Set());
        storeEnvironment(initialEnvironmentPointer, values);

        val arrayName = freshName("arrayPointer");
        val objName = freshName("obj");
        val pointerName = freshName("functionPointerPointer");
        val functionName = freshName("functionPointer");

        emit(ExtractValue(arrayName, transform(value), 0));
        emit(ExtractValue(objName, transform(value), 1));
        emit(GetElementPtr(pointerName, methodType, LocalReference(PointerType(), arrayName), List(tag)))
        emit(Load(functionName, methodType, LocalReference(PointerType(), pointerName)))
        emit(TailCall(LocalReference(methodType, functionName), List(LocalReference(objectType, objName), initialEnvironmentPointer, getStackPointer())));
        RetVoid()

      case machine.Allocate(ref @ machine.Variable(name, machine.Type.Reference(tpe)), init, evidence, rest) =>
        emit(Comment("statement allocate"))
        val idx = regionIndex(ref.tpe)

        val tmp = freshName("tmp")
        val tmpRef = LocalReference(StructureType(List(PointerType(), referenceType)), tmp)
        emit(Call(tmp, tmpRef.tpe, alloc, List(ConstantInt(idx), transform(evidence))));

        val ptr = freshName("pointer");
        val ptrRef = LocalReference(PointerType(), ptr)
        emit(ExtractValue(ptr, tmpRef, 0))

        emit(ExtractValue(name, tmpRef, 1))


        emit(Store(ptrRef, transform(init)))

        shareValues(List(init), freeVariables(rest))
        transform(rest);

      case machine.Allocate(_, _, _, _) =>
        ???

      case machine.Load(name, ref, ev, rest) =>
        emit(Comment("statement load"))
        val idx = regionIndex(ref.tpe)

        val ptr = freshName("pointer");
        val ptrRef = LocalReference(PointerType(), ptr)
        emit(Call(ptr, PointerType(), getPointer, List(transform(ref), ConstantInt(idx), transform(ev))))

        val oldVal = machine.Variable(freshName(ref.name + "_old"), name.tpe)
        emit(Load(oldVal.name, transform(oldVal.tpe), ptrRef))
        shareValue(oldVal)

        emit(Load(name.name, transform(name.tpe), ptrRef))
        eraseValues(List(name), freeVariables(rest))
        transform(rest)

      case machine.Store(ref, value, ev, rest) =>
        emit(Comment("statement store"))
        val idx = regionIndex(ref.tpe)

        val ptr = freshName("pointer");
        val ptrRef = LocalReference(PointerType(), ptr)
        emit(Call(ptr, PointerType(), getPointer, List(transform(ref), ConstantInt(idx), transform(ev))))

        val oldVal = machine.Variable(freshName(ref.name + "_old"), value.tpe)
        emit(Load(oldVal.name, transform(oldVal.tpe), ptrRef))
        eraseValue(oldVal)

        emit(Store(ptrRef, transform(value)))
        shareValues(List(value), freeVariables(rest))
        transform(rest)

      case machine.PushFrame(frame, rest) =>
        val frameEnvironment = freeVariables(frame).toList;

        val returnAddressName = freshName("returnAddress");
        val parameters = frame.parameters.map { case machine.Variable(name, tpe) => Parameter(transform(tpe), name) }
        defineFunction(returnAddressName, parameters :+ Parameter(stackPointerType, "stackPointer")) {
          emit(Comment("statement pushFrame / return address"))
          popEnvironment(frameEnvironment);
          // eraseValues(frameEnvironment, frameEnvironment) (unnecessary)
          eraseValues(frame.parameters, freeVariables(frame.body))

          transform(frame.body);
        }

        // TODO cache based on environment
        val sharerName = freshName("sharer");
        defineFunction(sharerName, List(Parameter(stackPointerType, "stackPointer"))) {
          emit(Comment("statement pushFrame / sharer"))
          popEnvironment(frameEnvironment);
          shareValues(frameEnvironment, Set.from(frameEnvironment));
          emit(TailCall(shareFrames, List(getStackPointer())));
          RetVoid()
        }

        // TODO cache based on environment (careful, this is different from other erasers)
        val eraserName = freshName("eraser");
        defineFunction(eraserName, List(Parameter(stackPointerType, "stackPointer"))) {
          emit(Comment("statement pushFrame / eraser"))
          popEnvironment(frameEnvironment);
          eraseValues(frameEnvironment, Set());
          emit(TailCall(eraseFrames, List(getStackPointer())));
          RetVoid()
        }

        shareValues(frameEnvironment, freeVariables(rest));
        pushEnvironment(frameEnvironment);
        pushReturnAddress(returnAddressName, sharerName, eraserName);

        transform(rest)

      case machine.Return(values) =>
        emit(Comment("statement return"))
        shareValues(values, Set())

        val returnAddress = popReturnAddress();
        emit(TailCall(LocalReference(returnAddressType, returnAddress), values.map(transform) :+ getStackPointer()));
        RetVoid()

        // this
      case machine.NewStack(variable, frame, rest) =>
        emit(Comment("statement newStack"))
        emit(Call(variable.name, transform(variable.tpe), newStack, List()));

        val frameEnvironment = freeVariables(frame).toList;

        val returnAddressName = freshName("returnAddress");
        defineFunction(returnAddressName, List(Parameter(environmentType, "environment"), Parameter(stackPointerType, "stackPointer"))) {
          emit(Comment("statement newStack / return address"))
          popEnvironment(frameEnvironment);
          // eraseValues(frameEnvironment, frameEnvironment) (unnecessary)
          loadEnvironment(initialEnvironmentPointer, frame.parameters);
          eraseValues(frame.parameters, freeVariables(frame.body));

          val newStackPointer = LocalReference(stackPointerType, freshName("stackPointer"));
          emit(Call(newStackPointer.name, stackPointerType, underflowStack, List(getStackPointer())));
          setStackPointer(newStackPointer);

          transform(frame.body);
        }

        // TODO cache based on environment (this is different from other sharers)
        val sharerName = freshName("sharer");
        defineFunction(sharerName, List(Parameter(stackPointerType, "stackPointer"))) {
          emit(Comment("statement newStack / sharer"))
          popEnvironment(frameEnvironment);
          shareValues(frameEnvironment, Set.from(frameEnvironment));
          RetVoid()
        }

        // TODO cache based on environment (careful, this is different from other erasers)
        val eraserName = freshName("eraser");
        defineFunction(eraserName, List(Parameter(stackPointerType, "stackPointer"))) {
          emit(Comment("statement newStack / eraser"))
          popEnvironment(frameEnvironment);
          eraseValues(frameEnvironment, Set());
          emit(Call("_", VoidType(), free, List(getStackPointer())));
          RetVoid()
        }

        shareValues(frameEnvironment, freeVariables(rest));
        val stackPointerPointer = LocalReference(PointerType(), freshName("stackPointPointer"));
        val oldStackPointer = LocalReference(stackPointerType, freshName("stackPointer"));
        emit(GetElementPtr(stackPointerPointer.name, NamedType("StackValue"), LocalReference(PointerType(), variable.name), List(0, 1, 0)));
        emit(Load(oldStackPointer.name, NamedType("StackPointer"), stackPointerPointer));
        val temporaryStackPointer = pushEnvironmentOnto(oldStackPointer, frameEnvironment);
        val newStackPointer = pushReturnAddressOnto(temporaryStackPointer, returnAddressName, sharerName, eraserName);
        emit(Store(stackPointerPointer, newStackPointer));

        eraseValues(List(variable), freeVariables(rest));
        transform(rest)

      case machine.PushStack(value, rest) =>
        emit(Comment("statement pushStack"))
        shareValues(List(value), freeVariables(rest));
        val newStackName = freshName("stack");
        emit(Call(newStackName, stackType, uniqueStack, List(transform(value))));
        val newStackPointerName = freshName("stackPointer");
        emit(Call(newStackPointerName, stackPointerType, pushStack, List(LocalReference(stackType, newStackName), getStackPointer())));
        setStackPointer(LocalReference(stackPointerType, newStackPointerName));
        transform(rest)

      case machine.PopStacks(variable, n, rest) =>
        emit(Comment("statement popStacks"))
        // TODO Handle n (n+1 = number of stacks to pop)
        val newStackPointerName = freshName("stackPointer");
        val tmpName = freshName("tmp");
        val tmpReference = LocalReference(StructureType(List(stackType, stackPointerType)), tmpName);
        emit(Call(tmpName, StructureType(List(stackType, stackPointerType)), popStacks, List(getStackPointer(), transform(n))));
        emit(ExtractValue(variable.name, tmpReference, 0));
        emit(ExtractValue(newStackPointerName, tmpReference, 1));
        setStackPointer(LocalReference(stackPointerType, newStackPointerName));

        eraseValues(List(variable), freeVariables(rest));
        transform(rest)

      case machine.ComposeEvidence(machine.Variable(name, _), ev1, ev2, rest) =>
        emit(Comment("statement composeEvidence"))
        emit(Add(name, transform(ev1), transform(ev2)))
        transform(rest)

      case machine.LiteralInt(machine.Variable(name, _), n, rest) =>
        emit(Comment("statement literalInt"))
        emit(Add(name, ConstantInt(n), ConstantInt(0)));
        transform(rest)

      case machine.LiteralDouble(machine.Variable(name, _), x, rest) =>
        emit(Comment("statement literalDouble"))
        emit(FAdd(name, ConstantDouble(x), ConstantDouble(0)));
        transform(rest)

      case machine.LiteralUTF8String(v@machine.Variable(bind, _), utf8, rest) =>
        emit(Comment("statement literalUTF8String"))
        emit(GlobalConstant(s"$bind.lit", ConstantArray(IntegerType8(), utf8.map { b => ConstantInteger8(b) }.toList)))

        val res = positiveType
        val args = List(ConstantInt(utf8.size), ConstantGlobal(PointerType(), s"$bind.lit"))
        val argsT = List(IntegerType64(), PointerType())
        emit(Call(bind, res, ConstantGlobal(FunctionType(res, argsT), "c_buffer_construct"), args))

        eraseValues(List(v), freeVariables(rest));
        transform(rest)

      case machine.LiteralEvidence(machine.Variable(name, _), n, rest) =>
        emit(Comment("statement literalEvidence"))
        emit(Add(name, ConstantInt(n), ConstantInt(0)));
        transform(rest)

      case machine.ForeignCall(machine.Variable(resultName, resultType), foreign, values, rest) =>
        emit(Comment("statement foreignCall"))
        // TODO careful with calling convention?!?
        val functionType = PointerType();
        shareValues(values, freeVariables(rest));
        emit(Call(resultName, transform(resultType), ConstantGlobal(functionType, foreign), values.map(transform)));
        transform(rest)

      case machine.Statement.Hole =>
        emit(Comment("statement Hole"))
        emit(Call("_", VoidType(), ConstantGlobal(FunctionType(VoidType(), Nil), "hole"), List.empty))
        RetVoid()
    }

  def transform(label: machine.Label): ConstantGlobal =
    label match {
      case machine.Label(name, _) => ConstantGlobal(PointerType(), name)
    }

  def transform(value: machine.Variable)(using FunctionContext): Operand =
    substitute(value) match {
      case machine.Variable(name, tpe) => LocalReference(transform(tpe), name)
    }

  def positiveType = NamedType("Pos");
  // TODO multiple methods (should be pointer to vtable)
  def negativeType = NamedType("Neg");
  def methodType = PointerType();
  def returnAddressType = NamedType("ReturnAddress");
  def sharerType = NamedType("Sharer");
  def eraserType = NamedType("Eraser");
  def frameHeaderType = NamedType("FrameHeader");
  def environmentType = NamedType("Environment");
  def objectType = NamedType("Object");
  def stackPointerType = NamedType("StackPointer");
  def stackType = NamedType("Stack");
  def referenceType = NamedType("Reference");

  def transform(tpe: machine.Type): Type = tpe match {
    case machine.Positive()          => positiveType
    case machine.Negative()          => negativeType
    case machine.Type.Int()          => IntegerType64()
    case machine.Type.Byte()         => IntegerType8()
    case machine.Type.Double()       => DoubleType()
    case machine.Type.String()       => positiveType
    case machine.Type.Stack()        => stackType
    case machine.Type.Reference(tpe) => referenceType
  }

  def environmentSize(environment: machine.Environment): Int =
    environment.map { case machine.Variable(_, typ) => typeSize(typ) }.sum

  def typeSize(tpe: machine.Type): Int =
    tpe match {
      case machine.Positive()        => 16
      case machine.Negative()        => 16
      case machine.Type.Int()        => 8 // TODO Make fat?
      case machine.Type.Byte()       => 1
      case machine.Type.Double()     => 8 // TODO Make fat?
      case machine.Type.String()     => 16
      case machine.Type.Stack()      => 8 // TODO Make fat?
      case machine.Type.Reference(_) => 8
    }

  def regionIndex(tpe: machine.Type): Int =
    tpe match {
          case machine.Type.Reference(machine.Type.Int()) => 0
          case machine.Type.Reference(machine.Type.Double()) => 0
          case machine.Type.Reference(machine.Type.Positive()) => 1
          case machine.Type.Reference(machine.Type.Negative()) => 1
          case machine.Type.Reference(machine.Type.String()) => 2
          case _ => ???
    }

  def defineFunction(name: String, parameters: List[Parameter])(prog: (FunctionContext, BlockContext) ?=> Terminator): ModuleContext ?=> Unit = {
    implicit val FC = FunctionContext();
    implicit val BC = BlockContext();

    val terminator = prog;

    val basicBlocks = FC.basicBlocks; FC.basicBlocks = null;
    val instructions = BC.instructions; BC.instructions = null;

    val entryBlock = BasicBlock("entry", instructions, terminator);
    val function = Function(VoidType(), name, parameters, entryBlock :: basicBlocks);

    emit(function)
  }

  def initialEnvironmentPointer = LocalReference(environmentType, "environment")

  def loadEnvironment(environmentPointer: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    if (environment.isEmpty) {
      ()
    } else {
      loadEnvironmentAt(environmentPointer, environment);
    }
  }

  def storeEnvironment(environmentPointer: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    if (environment.isEmpty) {
      ()
    } else {
      storeEnvironmentAt(environmentPointer, environment);
    }
  }

  def getEraser(environment: machine.Environment)(using C: ModuleContext): Operand = {
    val types = environment.map{ _.tpe };
    val freshEnvironment = environment.map{
      case machine.Variable(name, tpe) => machine.Variable(freshName(name), tpe)
    };
    val eraser = ConstantGlobal(eraserType, freshName("eraser"));

    C.erasers.getOrElseUpdate(types, {
      defineFunction(eraser.name, List(Parameter(environmentType, "environment"))) {
        // TODO avoid unnecessary loads
        loadEnvironmentAt(LocalReference(environmentType, "environment"), freshEnvironment);
        eraseValues(freshEnvironment, Set());
        RetVoid()
      };
      eraser
    });
  }

  def produceObject(environment: machine.Environment, freeInBody: Set[machine.Variable])(using ModuleContext, FunctionContext, BlockContext): Operand = {
    if (environment.isEmpty) {
      ConstantNull(objectType)
    } else {
      val obj = LocalReference(objectType, freshName("obj"));
      val env = LocalReference(environmentType, freshName("environment"));
      val size = ConstantInt(environmentSize(environment));
      val eraser = getEraser(environment)

      emit(Call(obj.name, objectType, newObject, List(eraser, size)));
      emit(Call(env.name, environmentType, objectEnvironment, List(obj)));
      shareValues(environment, freeInBody);
      storeEnvironment(env, environment);
      obj
    }
  }

  def consumeObject(obj: Operand, environment: machine.Environment, freeInBody: Set[machine.Variable])(using ModuleContext, FunctionContext, BlockContext): Unit = {
    if (environment.isEmpty) {
      ()
    } else {
      val env = LocalReference(environmentType, freshName("environment"));
      emit(Call(env.name, environmentType, objectEnvironment, List(obj)));
      loadEnvironment(env, environment);
      shareValues(environment, freeInBody);
      emit(Call("_", VoidType(), eraseObject, List(obj)));
    }
  }

  def pushEnvironment(environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    setStackPointer(pushEnvironmentOnto(getStackPointer(), environment))
  }

  def pushEnvironmentOnto(oldStackPointer: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Operand = {
    if (environment.isEmpty) {
      oldStackPointer
    } else {
      storeEnvironmentAt(oldStackPointer, environment);

      val newStackPointer = LocalReference(stackPointerType, freshName("stackPointer"));
      emit(GetElementPtr(newStackPointer.name, environmentType(environment), oldStackPointer, List(1)));

      newStackPointer
    }
  }

  def popEnvironment(environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    setStackPointer(popEnvironmentFrom(getStackPointer(), environment))
  }

  def popEnvironmentFrom(oldStackPointer: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Operand = {
    if (environment.isEmpty) {
      oldStackPointer
    } else {

      val newStackPointer = LocalReference(stackPointerType, freshName("stackPointer"));
      emit(GetElementPtr(newStackPointer.name, environmentType(environment), oldStackPointer, List(-1)));

      loadEnvironmentAt(newStackPointer, environment);

      newStackPointer
    }
  }

  def environmentType(environment: machine.Environment): Type =
    StructureType(environment.map {
      case machine.Variable(_, tpe) => transform(tpe)
    })

  def storeEnvironmentAt(pointer: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    val `type` = environmentType(environment)
    environment.zipWithIndex.foreach {
      case (machine.Variable(name, tpe), i) =>
        val field = LocalReference(PointerType(), freshName(name + "p"));
        emit(GetElementPtr(field.name, `type`, pointer, List(0, i)));
        emit(Store(field, transform(machine.Variable(name, tpe))))
    }
  }

  def loadEnvironmentAt(pointer: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    val `type` = environmentType(environment)
    environment.zipWithIndex.foreach {
      case (machine.Variable(name, tpe), i) =>
        val field = LocalReference(PointerType(), freshName(name + "p"));
        emit(GetElementPtr(field.name, `type`, pointer, List(0, i)));
        emit(Load(name, transform(tpe), field))
    }
  }

  def shareValues(values: machine.Environment, freeInBody: Set[machine.Variable])(using FunctionContext, BlockContext): Unit = {
    def loop(values: machine.Environment): Unit = {
      values match {
        case Nil => ()
        case value :: values =>
        if values.map(substitute).contains(substitute(value)) then {
          shareValue(value);
          loop(values)
        } else if freeInBody.map(substitute).contains(substitute(value)) then {
          shareValue(value);
          loop(values)
        } else {
          loop(values)
        }
      }
    };
    loop(values)
  }

  def eraseValues(environment: machine.Environment, freeInBody: Set[machine.Variable])(using ModuleContext, FunctionContext, BlockContext): Unit = {
    environment.map { value =>
      if !freeInBody.map(substitute).contains(substitute(value)) then {
        eraseValue(value)
      } else {
        ()
      }
    }
  }

  def shareValue(value: machine.Variable)(using FunctionContext, BlockContext): Unit = {
    value.tpe match {
      case machine.Positive()        => emit(Call("_", VoidType(), sharePositive, List(transform(value))))
      case machine.Negative()        => emit(Call("_", VoidType(), shareNegative, List(transform(value))))
      case machine.Type.Stack()      => emit(Call("_", VoidType(), shareStack, List(transform(value))))
      case machine.Type.Int()        => ()
      case machine.Type.Byte()       => ()
      case machine.Type.Double()     => ()
      case machine.Type.String()     => emit(Call("_", VoidType(), shareString, List(transform(value))))
      case machine.Type.Reference(_) => ()
    }
  }

  def eraseValue(value: machine.Variable)(using FunctionContext, BlockContext): Unit = {
    value.tpe match {
      case machine.Positive()        => emit(Call("_", VoidType(), erasePositive, List(transform(value))))
      case machine.Negative()        => emit(Call("_", VoidType(), eraseNegative, List(transform(value))))
      case machine.Type.Stack()      => emit(Call("_", VoidType(), eraseStack, List(transform(value))))
      case machine.Type.Int()        => ()
      case machine.Type.Byte()       => ()
      case machine.Type.Double()     => ()
      case machine.Type.String()     => emit(Call("_", VoidType(), eraseString, List(transform(value))))
      case machine.Type.Reference(_) => ()
    }
  }

  def pushReturnAddress(returnAddressName: String, scannerName: String, eraserName: String)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    setStackPointer(pushReturnAddressOnto(getStackPointer(), returnAddressName, scannerName, eraserName));
  }

  def pushReturnAddressOnto(oldStackPointer: Operand, returnAddressName: String, sharerName: String, eraserName: String)(using ModuleContext, FunctionContext, BlockContext): Operand = {

    val pointerType = PointerType();

    val returnAddressPointer = LocalReference(PointerType(), freshName("returnAddressPointer"));
    emit(GetElementPtr(returnAddressPointer.name, frameHeaderType, oldStackPointer, List(0, 0)));
    val sharerPointer = LocalReference(PointerType(), freshName("sharerPointer"));
    emit(GetElementPtr(sharerPointer.name, frameHeaderType, oldStackPointer, List(0, 1)));
    val eraserPointer = LocalReference(PointerType(), freshName("eraserPointer"));
    emit(GetElementPtr(eraserPointer.name, frameHeaderType, oldStackPointer, List(0, 2)));

    emit(Store(returnAddressPointer, ConstantGlobal(returnAddressType, returnAddressName)));
    emit(Store(sharerPointer, ConstantGlobal(sharerType, sharerName)));
    emit(Store(eraserPointer, ConstantGlobal(eraserType, eraserName)));

    val newStackPointer = LocalReference(stackPointerType, freshName("stackPointer"));
    emit(GetElementPtr(newStackPointer.name, frameHeaderType, oldStackPointer, List(1)));

    newStackPointer
  }

  def popReturnAddress()(using ModuleContext, FunctionContext, BlockContext): String = {
    val returnAddress = freshName("returnAddress");
    setStackPointer(popReturnAddressFrom(getStackPointer(), returnAddress));
    returnAddress
  }

  def popReturnAddressFrom(oldStackPointer: Operand, returnAddressName: String)(using ModuleContext, FunctionContext, BlockContext): Operand = {

    val newStackPointer = LocalReference(stackPointerType, freshName("stackPointer"));
    emit(GetElementPtr(newStackPointer.name, frameHeaderType, oldStackPointer, List(-1)));

    val returnAddressPointer = LocalReference(PointerType(), freshName("returnAddressPointer"));
    emit(GetElementPtr(returnAddressPointer.name, frameHeaderType, newStackPointer, List(0, 0)));

    emit(Load(returnAddressName, returnAddressType, returnAddressPointer));

    newStackPointer
  }

  def malloc = ConstantGlobal(PointerType(), "malloc");
  def free = ConstantGlobal(PointerType(), "free");

  def newObject = ConstantGlobal(PointerType(), "newObject");
  def objectEnvironment = ConstantGlobal(PointerType(), "objectEnvironment");

  def shareObject = ConstantGlobal(PointerType(), "shareObject");
  def sharePositive = ConstantGlobal(PointerType(), "sharePositive");
  def shareNegative = ConstantGlobal(PointerType(), "shareNegative");
  def shareStack = ConstantGlobal(PointerType(), "shareStack");
  def shareFrames = ConstantGlobal(PointerType(), "shareFrames");
  def shareString = ConstantGlobal(PointerType(), "sharePositive");

  def eraseObject = ConstantGlobal(PointerType(), "eraseObject");
  def erasePositive = ConstantGlobal(PointerType(), "erasePositive");
  def eraseNegative = ConstantGlobal(PointerType(), "eraseNegative");
  def eraseStack = ConstantGlobal(PointerType(), "eraseStack");
  def eraseFrames = ConstantGlobal(PointerType(), "eraseFrames");
  def eraseString = ConstantGlobal(PointerType(), "erasePositive");

  def alloc = ConstantGlobal(PointerType(), "alloc")
  def getPointer = ConstantGlobal(PointerType(), "getPointer")

  def newStack = ConstantGlobal(PointerType(), "newStack");
  def pushStack = ConstantGlobal(PointerType(), "pushStack");
  def popStacks = ConstantGlobal(PointerType(), "popStacks");
  def underflowStack = ConstantGlobal(PointerType(), "underflowStack");
  def uniqueStack = ConstantGlobal(PointerType(), "uniqueStack");


  /**
   * Extra info in context
   */
  class ModuleContext() {
    var counter = 0;
    var definitions: List[Definition] = List();
    val erasers = mutable.HashMap[List[machine.Type], Operand]();
  }

  def emit(definition: Definition)(using C: ModuleContext) =
    C.definitions = C.definitions :+ definition

  def freshName(name: String)(using C: ModuleContext): String = {
    C.counter = C.counter + 1;
    name + "_" + C.counter
  }

  class FunctionContext() {
    var substitution: Map[machine.Variable, machine.Variable] = Map();
    var basicBlocks: List[BasicBlock] = List();
  }

  def emit(basicBlock: BasicBlock)(using C: FunctionContext) =
    C.basicBlocks = C.basicBlocks :+ basicBlock

  def withBindings[R](bindings: List[(machine.Variable, machine.Variable)])(prog: () => R)(using C: FunctionContext): R = {
    val substitution = C.substitution;
    C.substitution = substitution ++ bindings.map { case (variable -> value) => (variable -> substitution.getOrElse(value, value) ) }.toMap;
    val result = prog();
    C.substitution = substitution
    result
  }

  def substitute(value: machine.Variable)(using C: FunctionContext): machine.Variable =
    C.substitution.toMap.getOrElse(value, value)

  class BlockContext() {
    var stackPointer: Operand = LocalReference(stackPointerType, "stackPointer");
    var instructions: List[Instruction] = List();
  }

  def emit(instruction: Instruction)(using C: BlockContext) =
    C.instructions = C.instructions :+ instruction

  def getStackPointer()(using C: BlockContext) =
    C.stackPointer

  def setStackPointer(stackPointer: Operand)(using C: BlockContext) =
    C.stackPointer = stackPointer;

  def escape(scalaString: String): String =
    scalaString.foldLeft(StringBuilder()) { (acc, c) =>
      escapeSeqs.get(c) match {
        case Some(s) => acc ++= s
        case None => acc += c
      }
    }.toString()
}
