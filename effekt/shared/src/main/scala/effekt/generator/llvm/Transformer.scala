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
      emit(Comment(s"program, ${declarations.length} declarations"))

      given MC: ModuleContext = ModuleContext();
      given FC: FunctionContext = FunctionContext();
      given BC: BlockContext = BlockContext();

      // TODO proper initialization of runtime
      emit(Call("environment", environmentType, malloc, List(ConstantInt(1024 * 1024))));
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
        defineFunction(name, List(Parameter(environmentType, "environment"), Parameter(stackPointerType, "stackPointer"))) {
          emit(Comment(s"statement definition $name, environment length ${environment.length}"))
          loadEnvironment(initialEnvironmentPointer, environment)
          eraseValues(environment, freeVariables(body))
          transform(body)
        }

        transform(rest)

      case machine.Jump(label) =>
        emit(Comment(s"statement jump ${label.name}"))
        shareValues(label.environment, Set())
        storeEnvironment(initialEnvironmentPointer, label.environment)

        emit(TailCall(transform(label), List(LocalReference(environmentType, "environment"), getStackPointer())))
        RetVoid()

      case machine.Substitute(bindings, rest) =>
        emit(Comment("statement substitution"))
        bindings.foreach { (from, to) => emit(Comment(s"substitution [${from.name} !-> ${to.name}]")) }
        withBindings(bindings) { () =>
          transform(rest)
        }

      case machine.Construct(variable, tag, values, rest) =>
        emit(Comment(s"statement construct ${variable.name}, tag ${tag}, ${values.length} values"))
        val `object` = produceObject(values, freeVariables(rest))
        val temporaryName = freshName(variable.name + "_temporary")
        emit(InsertValue(temporaryName, ConstantAggregateZero(positiveType), ConstantInt(tag), 0))
        emit(InsertValue(variable.name, LocalReference(positiveType, temporaryName), `object`, 1))

        eraseValues(List(variable), freeVariables(rest))
        transform(rest)

      case machine.Switch(value, clauses, default) =>
        emit(Comment(s"statement switch ${value.name}, ${clauses.length} clauses"))
        shareValues(List(value), clauses.flatMap(freeVariables).toSet)

        val tagName = freshName("tag")
        val objectName = freshName("object")
        emit(ExtractValue(tagName, transform(value), 0))
        emit(ExtractValue(objectName, transform(value), 1))

        val freeInClauses = clauses.flatMap(freeVariables)

        val stackPointer = getStackPointer();
        def labelClause(clause: machine.Clause): String = {
          implicit val BC = BlockContext()
          BC.stackPointer = stackPointer

          consumeObject(LocalReference(objectType, objectName), clause.parameters, freeVariables(clause.body));
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

      case machine.New(variable, clauses, rest) =>
        emit(Comment(s"statement new ${variable.name}, ${clauses.length} clauses"))
        val closureEnvironment = freeVariables(clauses).toList;

        val clauseNames = clauses.map { clause =>
          val clauseName = freshName(variable.name + "_clause")
          defineFunction(clauseName, List(Parameter(objectType, "object"), Parameter(environmentType, "environment"), Parameter(stackPointerType, "stackPointer"))) {
            emit(Comment(s"statement new ${clauseName}, ${clause.parameters.length} parameters"))
            consumeObject(LocalReference(objectType, "object"), closureEnvironment, freeVariables(clause));
            loadEnvironment(initialEnvironmentPointer, clause.parameters);
            eraseValues(clause.parameters, freeVariables(clause.body));
            transform(clause.body);
          }
          ConstantGlobal(methodType, clauseName)
        }

        val arrayName = freshName(variable.name + "_array")
        emit(GlobalConstant(arrayName, ConstantArray(methodType, clauseNames)))

        val `object` = produceObject(closureEnvironment, freeVariables(rest));
        val temporaryName = freshName(arrayName + "_temporary");
        emit(InsertValue(temporaryName, ConstantAggregateZero(negativeType), ConstantGlobal(PointerType(), arrayName), 0));
        emit(InsertValue(variable.name, LocalReference(negativeType, temporaryName), `object`, 1));

        eraseValues(List(variable), freeVariables(rest));
        transform(rest)

      case machine.Invoke(value, tag, values) =>
        emit(Comment(s"statement invoke ${value.name}, tag ${tag}, ${values.length} values"))
        shareValues(value :: values, Set());
        storeEnvironment(initialEnvironmentPointer, values);

        val arrayName = freshName("arrayPointer");
        val objectName = freshName("object");
        val pointerName = freshName("functionPointerPointer");
        val functionName = freshName("functionPointer");

        emit(ExtractValue(arrayName, transform(value), 0));
        emit(ExtractValue(objectName, transform(value), 1));
        emit(GetElementPtr(pointerName, methodType, LocalReference(PointerType(), arrayName), List(tag)))
        emit(Load(functionName, methodType, LocalReference(PointerType(), pointerName)))
        emit(TailCall(LocalReference(methodType, functionName), List(LocalReference(objectType, objectName), initialEnvironmentPointer, getStackPointer())));
        RetVoid()

      case machine.Allocate(ref @ machine.Variable(name, machine.Type.Reference(tpe)), init, evidence, rest) =>
        emit(Comment(s"statement allocate $name, type ${tpe}, init ${init.name}, evidence ${evidence.name}"))
        val idx = regionIndex(ref.tpe)

        val temporary = freshName("temporaryEvidence")
        val temporaryRef = LocalReference(StructureType(List(PointerType(), referenceType)), temporary)
        emit(Call(temporary, temporaryRef.tpe, alloc, List(ConstantInt(idx), transform(evidence))));

        val ptr = freshName("pointer");
        val ptrRef = LocalReference(PointerType(), ptr)
        emit(ExtractValue(ptr, temporaryRef, 0))

        emit(ExtractValue(name, temporaryRef, 1))


        emit(Store(ptrRef, transform(init)))

        shareValues(List(init), freeVariables(rest))
        transform(rest);

      case machine.Allocate(_, _, _, _) =>
        ???

      case machine.Load(name, ref, ev, rest) =>
        emit(Comment(s"statement load ${name.name}, reference ${ref.name}, evidence ${ev.name}"))
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
        emit(Comment(s"statement store ${ref.name}, value ${value.name}, evidence ${ev.name}"))
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
        defineFunction(returnAddressName, List(Parameter(environmentType, "environment"), Parameter(stackPointerType, "stackPointer"))) {
          emit(Comment(s"statement pushFrame / return address, ${frameEnvironment.length} free variables"))
          popEnvironment(frameEnvironment);
          // eraseValues(frameEnvironment, frameEnvironment) (unnecessary)
          loadEnvironment(initialEnvironmentPointer, frame.parameters);
          eraseValues(frame.parameters, freeVariables(frame.body))

          transform(frame.body);
        }

        // TODO cache based on environment
        val sharerName = freshName("sharer");
        defineFunction(sharerName, List(Parameter(stackPointerType, "stackPointer"))) {
          emit(Comment(s"statement pushFrame / sharer, ${frameEnvironment.length} free variables"))
          popEnvironment(frameEnvironment);
          shareValues(frameEnvironment, Set.from(frameEnvironment));
          emit(TailCall(shareFrames, List(getStackPointer())));
          RetVoid()
        }

        // TODO cache based on environment (careful, this is different from other erasers)
        val eraserName = freshName("eraser");
        defineFunction(eraserName, List(Parameter(stackPointerType, "stackPointer"))) {
          emit(Comment(s"statement pushFrame / eraser, ${frameEnvironment.length} free variables"))
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
        emit(Comment(s"statement return, ${values.length} values"))
        shareValues(values, Set())
        storeEnvironment(initialEnvironmentPointer, values);

        val returnAddress = popReturnAddress();
        emit(TailCall(LocalReference(returnAddressType, returnAddress), List(initialEnvironmentPointer, getStackPointer())));
        RetVoid()

      case machine.NewStack(variable, frame, rest) =>
        emit(Comment(s"statement newStack ${variable.name}"))
        emit(Call(variable.name, transform(variable.tpe), newStack, List()));

        val frameEnvironment = freeVariables(frame).toList;

        val returnAddressName = freshName("returnAddress");
        defineFunction(returnAddressName, List(Parameter(environmentType, "environment"), Parameter(stackPointerType, "stackPointer"))) {
          emit(Comment(s"statement newStack / return address, ${frameEnvironment.length} free variables"))
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
          emit(Comment(s"statement newStack / sharer, ${frameEnvironment.length} free variables"))
          popEnvironment(frameEnvironment);
          shareValues(frameEnvironment, Set.from(frameEnvironment));
          RetVoid()
        }

        // TODO cache based on environment (careful, this is different from other erasers)
        val eraserName = freshName("eraser");
        defineFunction(eraserName, List(Parameter(stackPointerType, "stackPointer"))) {
          emit(Comment(s"statement newStack / eraser, ${frameEnvironment.length} free variables"))
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
        emit(Comment(s"statement pushStack ${value.name}"))
        shareValues(List(value), freeVariables(rest));
        val newStackName = freshName("stack");
        emit(Call(newStackName, stackType, uniqueStack, List(transform(value))));
        val newStackPointerName = freshName("stackPointer");
        emit(Call(newStackPointerName, stackPointerType, pushStack, List(LocalReference(stackType, newStackName), getStackPointer())));
        setStackPointer(LocalReference(stackPointerType, newStackPointerName));
        transform(rest)

      case machine.PopStacks(variable, n, rest) =>
        emit(Comment(s"statement popStacks ${variable.name}, n=${n.name}"))
        // TODO Handle n (n+1 = number of stacks to pop)
        val newStackPointerName = freshName("stackPointer");
        val temporaryName = freshName(newStackPointerName + "_temporary");
        val temporaryReference = LocalReference(StructureType(List(stackType, stackPointerType)), temporaryName);
        emit(Call(temporaryName, StructureType(List(stackType, stackPointerType)), popStacks, List(getStackPointer(), transform(n))));
        emit(ExtractValue(variable.name, temporaryReference, 0));
        emit(ExtractValue(newStackPointerName, temporaryReference, 1));
        setStackPointer(LocalReference(stackPointerType, newStackPointerName));

        eraseValues(List(variable), freeVariables(rest));
        transform(rest)

      case machine.ComposeEvidence(machine.Variable(name, _), ev1, ev2, rest) =>
        emit(Comment(s"statement composeEvidence $name, evidence 1 ${ev1.name}, evidence 2 ${ev2.name}"))
        emit(Add(name, transform(ev1), transform(ev2)))
        transform(rest)

      case machine.LiteralInt(machine.Variable(name, _), n, rest) =>
        emit(Comment(s"statement literalInt $name, n=$n"))
        emit(Add(name, ConstantInt(n), ConstantInt(0)));
        transform(rest)

      case machine.LiteralDouble(machine.Variable(name, _), x, rest) =>
        emit(Comment(s"statement literalDouble $name, x=$x"))
        emit(FAdd(name, ConstantDouble(x), ConstantDouble(0)));
        transform(rest)

      case machine.LiteralUTF8String(v@machine.Variable(bind, _), utf8, rest) =>
        emit(Comment(s"statement literalUTF8String $bind, ${utf8.length} bytes"))
        emit(GlobalConstant(s"$bind.lit", ConstantArray(IntegerType8(), utf8.map { b => ConstantInteger8(b) }.toList)))

        val res = positiveType
        val args = List(ConstantInt(utf8.size), ConstantGlobal(PointerType(), s"$bind.lit"))
        val argsT = List(IntegerType64(), PointerType())
        emit(Call(bind, res, ConstantGlobal(FunctionType(res, argsT), "c_buffer_construct"), args))

        eraseValues(List(v), freeVariables(rest));
        transform(rest)

      case machine.LiteralEvidence(machine.Variable(name, _), n, rest) =>
        emit(Comment(s"statement literalEvidence $name, n=$n"))
        emit(Add(name, ConstantInt(n), ConstantInt(0)));
        transform(rest)

      case machine.ForeignCall(machine.Variable(resultName, resultType), foreign, values, rest) =>
        emit(Comment(s"statement foreignCall $resultName : $resultType, foreign $foreign, ${values.length} values"))
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
      val objectReference = LocalReference(objectType, freshName("object"));
      val environmentReference = LocalReference(environmentType, freshName("environment"));
      val size = ConstantInt(environmentSize(environment));
      val eraser = getEraser(environment)

      emit(Call(objectReference.name, objectType, newObject, List(eraser, size)));
      emit(Call(environmentReference.name, environmentType, objectEnvironment, List(objectReference)));
      shareValues(environment, freeInBody);
      storeEnvironment(environmentReference, environment);
      objectReference
    }
  }

  def consumeObject(`object`: Operand, environment: machine.Environment, freeInBody: Set[machine.Variable])(using ModuleContext, FunctionContext, BlockContext): Unit = {
    if (environment.isEmpty) {
      ()
    } else {
      val environmentReference = LocalReference(environmentType, freshName("environment"));
      emit(Call(environmentReference.name, environmentType, objectEnvironment, List(`object`)));
      loadEnvironment(environmentReference, environment);
      shareValues(environment, freeInBody);
      emit(Call("_", VoidType(), eraseObject, List(`object`)));
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
        val field = LocalReference(PointerType(), freshName(name + "_pointer"));
        emit(GetElementPtr(field.name, `type`, pointer, List(0, i)));
        emit(Store(field, transform(machine.Variable(name, tpe))))
    }
  }

  def loadEnvironmentAt(pointer: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    val `type` = environmentType(environment)
    environment.zipWithIndex.foreach {
      case (machine.Variable(name, tpe), i) =>
        val field = LocalReference(PointerType(), freshName(name + "_pointer"));
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
