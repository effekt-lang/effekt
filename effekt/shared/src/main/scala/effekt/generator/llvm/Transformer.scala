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

  def transform(program: machine.Program)(using ErrorReporter): List[Definition] = program match {
    case machine.Program(declarations, statement) =>
      emit(Comment(s"program, ${declarations.length} declarations"))

      given MC: ModuleContext = ModuleContext();
      given FC: FunctionContext = FunctionContext();
      given BC: BlockContext = BlockContext();

      emit(Call("stack", Ccc(), stackType, withEmptyStack, List()));

      val terminator = transform(statement);

      val definitions = MC.definitions; MC.definitions = null;
      val basicBlocks = FC.basicBlocks; FC.basicBlocks = null;
      val instructions = BC.instructions; BC.instructions = null;

      val transitionJump = Call("_", Tailcc(false), VoidType(), ConstantGlobal(FunctionType(VoidType(), Nil), "effektMainTailcc"), List())
      val transitionBlock = BasicBlock("transition", List(transitionJump), RetVoid())
      val transitionFunction = Function(Ccc(), VoidType(), "effektMain", List(), List(transitionBlock))

      val entryBlock = BasicBlock("entry", instructions, terminator)
      val effektMain = Function(Tailcc(true), VoidType(), "effektMainTailcc", List(), entryBlock :: basicBlocks)
      declarations.map(transform) ++ definitions :+ transitionFunction :+ effektMain
  }

  // context getters
  private def MC(using MC: ModuleContext): ModuleContext = MC
  private def FC(using FC: FunctionContext): FunctionContext = FC
  private def BC(using BC: BlockContext): BlockContext = BC

  def transform(declaration: machine.Declaration)(using ErrorReporter): Definition =
    declaration match {
      case machine.Extern(functionName, parameters, returnType, async, body) =>
        val transformedParameters = parameters.map { case machine.Variable(name, tpe) => Parameter(transform(tpe), name) }
        if (async) {
          VerbatimFunction(Tailcc(true), VoidType(), functionName, transformedParameters :+ Parameter(stackType, "stack"), transform(body))
        } else {
          VerbatimFunction(Ccc(), transform(returnType), functionName, transformedParameters, transform(body))
        }
      case machine.Include(ff, content) =>
        Verbatim("; declaration include" ++ content)
    }

  def transform(body: machine.ExternBody)(using ErrorReporter): String = body match {
    case machine.ExternBody.StringExternBody(_, contents) =>
      "; declaration extern\n    " ++ transform(contents)
    case u: machine.ExternBody.Unsupported =>
      u.report
      """call void @hole()
        |unreachable
        |""".stripMargin
    }

  def transform(template: Template[machine.Variable]): String = "; variable\n    " ++ intercalate(template.strings, template.args.map {
    case machine.Variable(name, tpe) => PrettyPrinter.localName(name)
  }).mkString

  def transform(statement: machine.Statement)(using ModuleContext, FunctionContext, BlockContext): Terminator =
    statement match {

      case machine.Def(machine.Label(name, environment), body, rest) =>
        val parameters = environment.map { case machine.Variable(name, tpe) => Parameter(transform(tpe), name) }
        defineLabel(name, parameters) {
          emit(Comment(s"definition $name, environment length ${environment.length}"))
          eraseValues(environment, freeVariables(body))
          transform(body)
        }

        transform(rest)

      case machine.Jump(label) =>
        emit(Comment(s"jump ${label.name}"))
        shareValues(label.environment, Set())

        val arguments = label.environment.map(transform)
        emit(callLabel(transform(label), arguments))
        RetVoid()

      case machine.Substitute(bindings, rest) =>
        emit(Comment("substitution"))
        bindings.foreach { (from, to) => emit(Comment(s"substitution [${from.name} !-> ${to.name}]")) }
        withBindings(bindings) { () =>
          transform(rest)
        }

      case machine.Construct(variable, tag, values, rest) =>
        emit(Comment(s"construct ${variable.name}, tag ${tag}, ${values.length} values"))
        val fields = produceObject("fields", values, freeVariables(rest))
        val temporaryName = freshName(variable.name + "_temporary")
        emit(InsertValue(temporaryName, ConstantAggregateZero(positiveType), ConstantInt(tag), 0))
        emit(InsertValue(variable.name, LocalReference(positiveType, temporaryName), fields, 1))

        eraseValues(List(variable), freeVariables(rest))
        transform(rest)

      case machine.Switch(value, clauses, default) =>
        emit(Comment(s"switch ${value.name}, ${clauses.length} clauses"))
        shareValues(List(value), clauses.flatMap(freeVariables).toSet)

        val tagName = freshName("tag")
        val objectName = freshName("fields")
        emit(ExtractValue(tagName, transform(value), 0))
        emit(ExtractValue(objectName, transform(value), 1))

        val freeInClauses = clauses.flatMap(freeVariables)

        val stack = getStack()
        def labelClause(clause: machine.Clause): String = {
          implicit val BC = BlockContext()
          BC.stack = stack

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
        val closureEnvironment = freeVariables(clauses).toList;
        emit(Comment(s"new ${variable.name}, ${clauses.length} clauses, ${closureEnvironment.size} free variables"))

        val clauseNames = clauses.map { clause =>
          val clauseName = freshName(variable.name + "_clause");
          val parameters = clause.parameters.map { case machine.Variable(name, tpe) => Parameter(transform(tpe), name) }
          defineLabel(clauseName, Parameter(objectType, "closure") +: parameters) {
            emit(Comment(s"new ${clauseName}, ${clause.parameters.length} parameters"))
            consumeObject(LocalReference(objectType, "closure"), closureEnvironment, freeVariables(clause));
            eraseValues(clause.parameters, freeVariables(clause.body));
            transform(clause.body);
          }
          ConstantGlobal(methodType, clauseName)
        }

        val vtableName = freshName("vtable")
        emit(GlobalConstant(vtableName, ConstantArray(methodType, clauseNames)))

        val vtable = produceObject("closure", closureEnvironment, freeVariables(rest));
        val temporaryName = freshName("vtable_temporary");
        emit(InsertValue(temporaryName, ConstantAggregateZero(negativeType), ConstantGlobal(PointerType(), vtableName), 0));
        emit(InsertValue(variable.name, LocalReference(negativeType, temporaryName), vtable, 1));

        eraseValues(List(variable), freeVariables(rest));
        transform(rest)

      case machine.Invoke(value, tag, values) =>
        emit(Comment(s"invoke ${value.name}, tag ${tag}, ${values.length} values"))
        shareValues(value :: values, Set());

        val vtableName = freshName("vtable");
        val objectName = freshName("closure");
        val pointerName = freshName("functionPointer_pointer");
        val functionName = freshName("functionPointer");
        val arguments = values.map(transform)

        emit(ExtractValue(vtableName, transform(value), 0));
        emit(ExtractValue(objectName, transform(value), 1));
        emit(GetElementPtr(pointerName, methodType, LocalReference(PointerType(), vtableName), List(tag)))
        emit(Load(functionName, methodType, LocalReference(PointerType(), pointerName)))
        emit(callLabel(LocalReference(methodType, functionName), LocalReference(objectType, objectName) +: arguments))
        RetVoid()

      case machine.Allocate(reference, init, region, rest) =>
        emit(Comment(s"allocate ${reference.name}, type ${reference.tpe}, init ${init.name}, region ${region.name}"))
        val idx = regionIndex(reference.tpe)

        val temporaryRef = LocalReference(StructureType(List(PointerType(), referenceType)), freshName("cell"))
        emit(Call(temporaryRef.name, Ccc(), temporaryRef.tpe, alloc, List(ConstantInt(idx), transform(region), getStack())));

        val ptrRef = LocalReference(PointerType(), freshName("pointer"))
        emit(ExtractValue(ptrRef.name, temporaryRef, 0))

        emit(ExtractValue(reference.name, temporaryRef, 1))

        emit(Store(ptrRef, transform(init)))

        shareValues(List(init), freeVariables(rest))
        transform(rest)

      case machine.Load(name, reference, rest) =>
        emit(Comment(s"load ${name.name}, reference ${reference.name}"))

        val idx = regionIndex(reference.tpe)

        val ptrRef = LocalReference(PointerType(), freshName(name.name + "_pointer"))
        emit(Call(ptrRef.name, Ccc(), PointerType(), getPointer, List(transform(reference), ConstantInt(idx), getStack())))

        // We have to share the old value since now there exists a new reference to it
        val oldVal = machine.Variable(freshName(reference.name + "_old"), name.tpe)
        emit(Load(oldVal.name, transform(oldVal.tpe), ptrRef))
        shareValue(oldVal)

        emit(Load(name.name, transform(name.tpe), ptrRef))
        eraseValues(List(name), freeVariables(rest))
        transform(rest)

      case machine.Store(reference, value, rest) =>
        emit(Comment(s"store ${reference.name}, value ${value.name}"))
        val idx = regionIndex(reference.tpe)

        val ptrRef = LocalReference(PointerType(), freshName(reference.name + "pointer"))
        emit(Call(ptrRef.name, Ccc(), PointerType(), getPointer, List(transform(reference), ConstantInt(idx), getStack())))

        val oldVal = machine.Variable(freshName(reference.name + "_old"), value.tpe)
        emit(Load(oldVal.name, transform(oldVal.tpe), ptrRef))
        eraseValue(oldVal)

        emit(Store(ptrRef, transform(value)))
        shareValues(List(value), freeVariables(rest))
        transform(rest)

      case machine.Var(ref @ machine.Variable(name, machine.Type.Reference(tpe)), init, retType, rest) =>
        val environment = List(init)
        val returnAddressName = freshName("returnAddress")
        val returnValue = freshName("returnValue")
        val returnType = transform(retType)
        val parameters = List(Parameter(returnType, returnValue))

        defineLabel(returnAddressName, parameters) {
          emit(Comment(s"var $name / return address"))
          popEnvironmentFrom(getStack(), environment)
          eraseValue(init)
          val nextReturn = LocalReference(returnAddressType, freshName("returnAddress"))
          popReturnAddressFrom(getStack(), nextReturn.name)
          emit(callLabel(nextReturn, List(LocalReference(returnType, returnValue))))
          RetVoid()
        }

        val sharerName = freshName("sharer");
        defineFunction(sharerName, List(Parameter(stackPointerType, "stackPointer"))) {
          emit(Comment(s"sharer, ${environment.length} free variables"))

          val nextStackPointer = LocalReference(stackPointerType, freshName("stackPointer"));
          emit(GetElementPtr(nextStackPointer.name, environmentType(environment), LocalReference(stackPointerType, "stackPointer"), List(-1)));
          loadEnvironmentAt(nextStackPointer, environment);

          shareValues(environment, Set.from(environment));
          emit(Call("_", Ccc(), VoidType(), shareFrames, List(nextStackPointer)));
          RetVoid()
        }

        val eraserName = freshName("eraser");
        defineFunction(eraserName, List(Parameter(stackPointerType, "stackPointer"))) {
          emit(Comment(s"eraser, ${environment.length} free variables"))

          val nextStackPointer = LocalReference(stackPointerType, freshName("stackPointer"));
          emit(GetElementPtr(nextStackPointer.name, environmentType(environment), LocalReference(stackPointerType, "stackPointer"), List(-1)));
          loadEnvironmentAt(nextStackPointer, environment);

          eraseValues(environment, Set());
          emit(Call("_", Ccc(), VoidType(), eraseFrames, List(nextStackPointer)));
          RetVoid()
        }

        emit(Call(name, Ccc(), referenceType, newReference, List(getStack())))

        shareValues(environment, freeVariables(rest));
        pushEnvironmentOnto(getStack(), environment);
        pushReturnAddressOnto(getStack(), returnAddressName, sharerName, eraserName);

        transform(rest)

      case machine.Var(_, _, _, _) => ???

      case machine.LoadVar(name, ref, rest) =>
        emit(Comment(s"loadvar ${name.name}, reference ${ref.name}"))

        val ptr = freshName(name.name + "_pointer");
        val ptrRef = LocalReference(PointerType(), ptr)
        emit(Call(ptr, Ccc(), PointerType(), getVarPointer, List(transform(ref), getStack())))

        // TODO why do we need this?
        val oldVal = machine.Variable(freshName(ref.name + "_old"), name.tpe)
        emit(Load(oldVal.name, transform(oldVal.tpe), ptrRef))
        shareValue(oldVal)

        emit(Load(name.name, transform(name.tpe), ptrRef))
        eraseValues(List(name), freeVariables(rest))
        transform(rest)

      case machine.StoreVar(ref, value, rest) =>
        emit(Comment(s"storevar ${ref.name}, value ${value.name}"))

        val ptr = freshName(ref.name + "pointer");
        val ptrRef = LocalReference(PointerType(), ptr)
        emit(Call(ptr, Ccc(), PointerType(), getVarPointer, List(transform(ref), getStack())))

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
        defineLabel(returnAddressName, parameters) {
          emit(Comment(s"pushFrame / return address, ${frameEnvironment.length} free variables"))
          popEnvironmentFrom(getStack(), frameEnvironment);
          // eraseValues(frameEnvironment, frameEnvironment) (unnecessary)
          eraseValues(frame.parameters, freeVariables(frame.body))

          transform(frame.body);
        }

        // TODO cache based on environment
        val sharerName = freshName("sharer");
        defineFunction(sharerName, List(Parameter(stackPointerType, "stackPointer"))) {
          emit(Comment(s"pushFrame / sharer, ${frameEnvironment.length} free variables"))

          val nextStackPointer = LocalReference(stackPointerType, freshName("stackPointer"));
          emit(GetElementPtr(nextStackPointer.name, environmentType(frameEnvironment), LocalReference(stackPointerType, "stackPointer"), List(-1)));
          loadEnvironmentAt(nextStackPointer, frameEnvironment);

          shareValues(frameEnvironment, Set.from(frameEnvironment));
          emit(Call("_", Ccc(), VoidType(), shareFrames, List(nextStackPointer)));
          RetVoid()
        }

        // TODO cache based on environment (careful, this is different from other erasers)
        val eraserName = freshName("eraser");
        defineFunction(eraserName, List(Parameter(stackPointerType, "stackPointer"))) {
          emit(Comment(s"pushFrame / eraser, ${frameEnvironment.length} free variables"))

          val nextStackPointer = LocalReference(stackPointerType, freshName("stackPointer"));
          emit(GetElementPtr(nextStackPointer.name, environmentType(frameEnvironment), LocalReference(stackPointerType, "stackPointer"), List(-1)));
          loadEnvironmentAt(nextStackPointer, frameEnvironment);

          eraseValues(frameEnvironment, Set());
          emit(Call("_", Ccc(), VoidType(), eraseFrames, List(nextStackPointer)));
          RetVoid()
        }

        shareValues(frameEnvironment, freeVariables(rest));
        pushEnvironmentOnto(getStack(), frameEnvironment);
        pushReturnAddressOnto(getStack(), returnAddressName, sharerName, eraserName);

        transform(rest)

      case machine.Return(values) =>
        emit(Comment(s"return, ${values.length} values"))
        shareValues(values, Set())

        val returnAddress = LocalReference(returnAddressType, freshName("returnAddress"));
        popReturnAddressFrom(getStack(), returnAddress.name);
        emit(callLabel(returnAddress, values.map(transform)))
        RetVoid()

      case machine.NewStack(variable, prompt, frame, rest) =>
        emit(Comment(s"newStack ${variable.name}"))
        emit(Call(variable.name, Ccc(), transform(variable.tpe), newStack, List(transform(prompt))));

        val frameEnvironment = freeVariables(frame).toList;

        val returnAddressName = freshName("returnAddress");
        val parameters = frame.parameters.map { case machine.Variable(name, tpe) => Parameter(transform(tpe), name) }
        defineLabel(returnAddressName, parameters) {
          emit(Comment(s"newStack / return address, ${frameEnvironment.length} free variables"))
          popEnvironmentFrom(getStack(), frameEnvironment);
          // eraseValues(frameEnvironment, frameEnvironment) (unnecessary)
          eraseValues(frame.parameters, freeVariables(frame.body));

          val nextStack = LocalReference(stackType, freshName("stack"));
          emit(Call(nextStack.name, Ccc(), nextStack.tpe, underflowStack, List(getStack())));
          setStack(nextStack);

          transform(frame.body);
        }

        // TODO cache based on environment (this is different from other sharers)
        val sharerName = freshName("sharer");
        defineFunction(sharerName, List(Parameter(stackPointerType, "stackPointer"))) {
          emit(Comment(s"newStack / sharer, ${frameEnvironment.length} free variables"))

          val nextStackPointer = LocalReference(stackPointerType, freshName("stackPointer"));
          emit(GetElementPtr(nextStackPointer.name, environmentType(frameEnvironment), LocalReference(stackPointerType, "stackPointer"), List(-1)));
          loadEnvironmentAt(nextStackPointer, frameEnvironment);

          shareValues(frameEnvironment, Set.from(frameEnvironment));
          RetVoid()
        }

        // TODO cache based on environment (careful, this is different from other erasers)
        val eraserName = freshName("eraser");
        defineFunction(eraserName, List(Parameter(stackPointerType, "stackPointer"))) {
          emit(Comment(s"newStack / eraser, ${frameEnvironment.length} free variables"))


          val nextStackPointer = LocalReference(stackPointerType, freshName("stackPointer"));
          emit(GetElementPtr(nextStackPointer.name, environmentType(frameEnvironment), LocalReference(stackPointerType, "stackPointer"), List(-1)));
          loadEnvironmentAt(nextStackPointer, frameEnvironment);

          eraseValues(frameEnvironment, Set());
          // TODO consider doing the following in the RTS
          emit(Call("_", Ccc(), VoidType(), free, List(nextStackPointer)));
          RetVoid()
        }

        shareValues(frameEnvironment, freeVariables(rest));

        val stack = LocalReference(stackType, variable.name);
        pushEnvironmentOnto(stack, frameEnvironment);
        pushReturnAddressOnto(stack, returnAddressName, sharerName, eraserName);

        eraseValues(List(variable), freeVariables(rest));
        transform(rest)

      case machine.PushStack(value, rest) =>
        emit(Comment(s"pushStack ${value.name}"))
        shareValues(List(value), freeVariables(rest));
        val newStackName = freshName("stack");
        emit(Call(newStackName, Ccc(), stackType, uniqueStack, List(transform(value))));
        emit(Call("_", Ccc(), VoidType(), pushStack, List(LocalReference(stackType, newStackName), getStack())));
        setStack(LocalReference(stackType, newStackName));
        transform(rest)

      case machine.PopStacks(variable, prompt, rest) =>
        emit(Comment(s"popStacks ${variable.name}, prompt=${prompt.name}"))
        val newStackName = freshName("stack");
        emit(Call(newStackName, Ccc(), stackType, popStacks, List(getStack(), transform(prompt))));

        emit(BitCast(variable.name, getStack(), stackType));
        setStack(LocalReference(stackType, newStackName));

        eraseValues(List(variable), freeVariables(rest));
        transform(rest)

      case machine.FreshPrompt(machine.Variable(name, _), rest) =>
        emit(Call(name, Ccc(), promptType, freshPrompt, Nil))
        transform(rest)

      case machine.CurrentPrompt(machine.Variable(name, _), rest) =>
        emit(Call(name, Ccc(), promptType, currentPrompt, List(getStack())))
        transform(rest)

      case machine.LiteralInt(machine.Variable(name, _), n, rest) =>
        emit(Comment(s"literalInt $name, n=$n"))
        emit(Add(name, ConstantInt(n), ConstantInt(0)));
        transform(rest)

      case machine.LiteralDouble(machine.Variable(name, _), x, rest) =>
        emit(Comment(s"literalDouble $name, x=$x"))
        emit(FAdd(name, ConstantDouble(x), ConstantDouble(0)));
        transform(rest)

      case machine.LiteralUTF8String(v@machine.Variable(bind, _), utf8, rest) =>
        emit(Comment(s"literalUTF8String $bind, ${utf8.length} bytes"))
        emit(GlobalConstant(s"$bind.lit", ConstantArray(IntegerType8(), utf8.map { b => ConstantInteger8(b) }.toList)))

        val res = positiveType
        val args = List(ConstantInt(utf8.size), ConstantGlobal(PointerType(), s"$bind.lit"))
        val argsT = List(IntegerType64(), PointerType())
        emit(Call(bind, Ccc(), res, ConstantGlobal(FunctionType(res, argsT), "c_buffer_construct"), args))

        eraseValues(List(v), freeVariables(rest));
        transform(rest)

      case machine.ForeignCall(machine.Variable(resultName, resultType), foreign, values, rest) =>
        emit(Comment(s"foreignCall $resultName : $resultType, foreign $foreign, ${values.length} values"))
        val functionType = PointerType();
        shareValues(values, freeVariables(rest));
        emit(Call(resultName, Ccc(), transform(resultType), ConstantGlobal(functionType, foreign), values.map(transform)));
        transform(rest)

      case machine.Statement.Hole =>
        emit(Comment("Hole"))
        emit(Call("_", Ccc(), VoidType(), ConstantGlobal(FunctionType(VoidType(), Nil), "hole"), List.empty))
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

  val positiveType = NamedType("Pos");
  // TODO multiple methods (should be pointer to vtable)
  val negativeType = NamedType("Neg");
  val methodType = PointerType();
  val returnAddressType = NamedType("ReturnAddress");
  val sharerType = NamedType("Sharer");
  val eraserType = NamedType("Eraser");
  val frameHeaderType = NamedType("FrameHeader");
  val environmentType = NamedType("Environment");
  val objectType = NamedType("Object");
  val stackPointerType = NamedType("StackPointer");
  val stackType = NamedType("Stack");
  val promptType = NamedType("Prompt");
  val referenceType = NamedType("Reference");

  def transform(tpe: machine.Type): Type = tpe match {
    case machine.Positive()          => positiveType
    case machine.Negative()          => negativeType
    case machine.Type.Prompt()       => promptType
    case machine.Type.Stack()        => stackType
    case machine.Type.Int()          => IntegerType64()
    case machine.Type.Byte()         => IntegerType8()
    case machine.Type.Double()       => DoubleType()
    case machine.Type.String()       => positiveType
    case machine.Type.Reference(tpe) => referenceType
  }

  def environmentSize(environment: machine.Environment): Int =
    environment.map { case machine.Variable(_, typ) => typeSize(typ) }.sum

  def typeSize(tpe: machine.Type): Int =
    tpe match {
      case machine.Positive()        => 16
      case machine.Negative()        => 16
      case machine.Type.Prompt()     => 8 // TODO Make fat?
      case machine.Type.Stack()      => 8 // TODO Make fat?
      case machine.Type.Int()        => 8 // TODO Make fat?
      case machine.Type.Byte()       => 1
      case machine.Type.Double()     => 8 // TODO Make fat?
      case machine.Type.String()     => 16
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
    val function = Function(Ccc(), VoidType(), name, parameters, entryBlock :: basicBlocks);

    emit(function)
  }

  def defineLabel(name: String, parameters: List[Parameter])(prog: (FunctionContext, BlockContext) ?=> Terminator): ModuleContext ?=> Unit = {
    implicit val FC = FunctionContext();
    implicit val BC = BlockContext();

    val terminator = prog;

    val basicBlocks = FC.basicBlocks; FC.basicBlocks = null;
    val instructions = BC.instructions; BC.instructions = null;

    val entryBlock = BasicBlock("entry", instructions, terminator);
    val function = Function(Tailcc(true), VoidType(), name, parameters :+ Parameter(stackType, "stack"), entryBlock :: basicBlocks);

    emit(function)
  }

  def callLabel(name: Operand, arguments: List[Operand])(using BlockContext): Instruction =
    Call("_", Tailcc(true), VoidType(), name, arguments :+ getStack())

  def callLabelTransition(name: Operand, arguments: List[Operand])(using BlockContext): Instruction =
    Call("_", Tailcc(false), VoidType(), name, arguments :+ getStack())

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

  def produceObject(role: String, environment: machine.Environment, freeInBody: Set[machine.Variable])(using ModuleContext, FunctionContext, BlockContext): Operand = {
    if (environment.isEmpty) {
      ConstantNull(objectType)
    } else {
      val objectReference = LocalReference(objectType, freshName(role));
      val environmentReference = LocalReference(environmentType, freshName("environment"));
      val size = ConstantInt(environmentSize(environment));
      val eraser = getEraser(environment)

      emit(Call(objectReference.name, Ccc(), objectType, newObject, List(eraser, size)));
      emit(Call(environmentReference.name, Ccc(), environmentType, objectEnvironment, List(objectReference)));
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
      emit(Call(environmentReference.name, Ccc(), environmentType, objectEnvironment, List(`object`)));
      loadEnvironment(environmentReference, environment);
      shareValues(environment, freeInBody);
      emit(Call("_", Ccc(), VoidType(), eraseObject, List(`object`)));
    }
  }

  def pushEnvironmentOnto(stack: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    if (environment.isEmpty) {
      ()
    } else {
      val stackPointer = LocalReference(stackPointerType, freshName("stackPointer"));
      val size = ConstantInt(environmentSize(environment));
      emit(Call(stackPointer.name, Ccc(), stackPointer.tpe, stackAllocate, List(stack, size)));
      storeEnvironmentAt(stackPointer, environment);
    }
  }

  def popEnvironmentFrom(stack: Operand, environment: machine.Environment)(using ModuleContext, FunctionContext, BlockContext): Unit = {
    if (environment.isEmpty) {
      ()
    } else {
      val stackPointer = LocalReference(stackPointerType, freshName("stackPointer"));
      val size = ConstantInt(environmentSize(environment));
      emit(Call(stackPointer.name, Ccc(), stackPointer.tpe, stackDeallocate, List(stack, size)));
      loadEnvironmentAt(stackPointer, environment)
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

  def eraseValues(environment: machine.Environment, freeInBody: Set[machine.Variable])(using ModuleContext, FunctionContext, BlockContext): Unit =
    environment.foreach { value =>
      if !freeInBody.map(substitute).contains(substitute(value)) then eraseValue(value)
    }

  def shareValue(value: machine.Variable)(using FunctionContext, BlockContext): Unit = {
    value.tpe match {
      case machine.Positive()        => emit(Call("_", Ccc(), VoidType(), sharePositive, List(transform(value))))
      case machine.Negative()        => emit(Call("_", Ccc(), VoidType(), shareNegative, List(transform(value))))
      case machine.Type.Prompt()     => ()
      case machine.Type.Stack()      => emit(Call("_", Ccc(), VoidType(), shareStack, List(transform(value))))
      case machine.Type.Int()        => ()
      case machine.Type.Byte()       => ()
      case machine.Type.Double()     => ()
      case machine.Type.String()     => emit(Call("_", Ccc(), VoidType(), shareString, List(transform(value))))
      case machine.Type.Reference(_) => ()
    }
  }

  def eraseValue(value: machine.Variable)(using FunctionContext, BlockContext): Unit = {
    value.tpe match {
      case machine.Positive()        => emit(Call("_", Ccc(), VoidType(), erasePositive, List(transform(value))))
      case machine.Negative()        => emit(Call("_", Ccc(), VoidType(), eraseNegative, List(transform(value))))
      case machine.Type.Prompt()     => ()
      case machine.Type.Stack()      => emit(Call("_", Ccc(), VoidType(), eraseStack, List(transform(value))))
      case machine.Type.Int()        => ()
      case machine.Type.Byte()       => ()
      case machine.Type.Double()     => ()
      case machine.Type.String()     => emit(Call("_", Ccc(), VoidType(), eraseString, List(transform(value))))
      case machine.Type.Reference(_) => ()
    }
  }

  def pushReturnAddressOnto(stack: Operand, returnAddressName: String, sharerName: String, eraserName: String)(using ModuleContext, FunctionContext, BlockContext): Unit = {

    val stackPointer = LocalReference(stackPointerType, freshName("stackPointer"));
    // TODO properly find size
    val size = ConstantInt(24);
    emit(Call(stackPointer.name, Ccc(), stackPointer.tpe, stackAllocate, List(stack, size)));

    val returnAddressPointer = LocalReference(PointerType(), freshName("returnAddress_pointer"));
    emit(GetElementPtr(returnAddressPointer.name, frameHeaderType, stackPointer, List(0, 0)));
    val sharerPointer = LocalReference(PointerType(), freshName("sharer_pointer"));
    emit(GetElementPtr(sharerPointer.name, frameHeaderType, stackPointer, List(0, 1)));
    val eraserPointer = LocalReference(PointerType(), freshName("eraser_pointer"));
    emit(GetElementPtr(eraserPointer.name, frameHeaderType, stackPointer, List(0, 2)));

    emit(Store(returnAddressPointer, ConstantGlobal(returnAddressType, returnAddressName)));
    emit(Store(sharerPointer, ConstantGlobal(sharerType, sharerName)));
    emit(Store(eraserPointer, ConstantGlobal(eraserType, eraserName)));
  }

  def popReturnAddressFrom(stack: Operand, returnAddressName: String)(using ModuleContext, FunctionContext, BlockContext): Unit = {

    val stackPointer = LocalReference(stackPointerType, freshName("stackPointer"));
    // TODO properly find size
    val size = ConstantInt(24);
    emit(Call(stackPointer.name, Ccc(), stackPointer.tpe, stackDeallocate, List(stack, size)));

    val returnAddressPointer = LocalReference(PointerType(), freshName("returnAddress_pointer"));
    emit(GetElementPtr(returnAddressPointer.name, frameHeaderType, stackPointer, List(0, 0)));

    emit(Load(returnAddressName, returnAddressType, returnAddressPointer));
  }

  val malloc = ConstantGlobal(PointerType(), "malloc");
  val free = ConstantGlobal(PointerType(), "free");

  val newObject = ConstantGlobal(PointerType(), "newObject");
  val objectEnvironment = ConstantGlobal(PointerType(), "objectEnvironment");

  val shareObject = ConstantGlobal(PointerType(), "shareObject");
  val sharePositive = ConstantGlobal(PointerType(), "sharePositive");
  val shareNegative = ConstantGlobal(PointerType(), "shareNegative");
  val shareStack = ConstantGlobal(PointerType(), "shareStack");
  val shareFrames = ConstantGlobal(PointerType(), "shareFrames");
  val shareString = ConstantGlobal(PointerType(), "sharePositive");

  val eraseObject = ConstantGlobal(PointerType(), "eraseObject");
  val erasePositive = ConstantGlobal(PointerType(), "erasePositive");
  val eraseNegative = ConstantGlobal(PointerType(), "eraseNegative");
  val eraseStack = ConstantGlobal(PointerType(), "eraseStack");
  val eraseFrames = ConstantGlobal(PointerType(), "eraseFrames");
  val eraseString = ConstantGlobal(PointerType(), "erasePositive");

  val alloc = ConstantGlobal(PointerType(), "alloc")
  val getPointer = ConstantGlobal(PointerType(), "getPointer")

  val newReference = ConstantGlobal(PointerType(), "newReference")
  val getVarPointer = ConstantGlobal(PointerType(), "getVarPointer")

  val newStack = ConstantGlobal(PointerType(), "newStack");
  val pushStack = ConstantGlobal(PointerType(), "pushStack");
  val popStacks = ConstantGlobal(PointerType(), "popStacks");
  val freshPrompt = ConstantGlobal(PointerType(), "freshPrompt");
  val currentPrompt = ConstantGlobal(PointerType(), "currentPrompt");
  val underflowStack = ConstantGlobal(PointerType(), "underflowStack");
  val uniqueStack = ConstantGlobal(PointerType(), "uniqueStack");
  val withEmptyStack = ConstantGlobal(PointerType(), "withEmptyStack");
  val stackAllocate = ConstantGlobal(PointerType(), "stackAllocate");
  val stackDeallocate = ConstantGlobal(PointerType(), "stackDeallocate");

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
    var stack: Operand = LocalReference(stackType, "stack");
    var instructions: List[Instruction] = List();
  }

  def emit(instruction: Instruction)(using C: BlockContext) =
    C.instructions = C.instructions :+ instruction

  def getStack()(using C: BlockContext) =
    C.stack

  def setStack(stack: Operand)(using C: BlockContext) =
    C.stack = stack;

  val escapeSeqs: Map[Char, String] = Map('\'' -> raw"'", '\"' -> raw"\"", '\\' -> raw"\\", '\n' -> raw"\n", '\t' -> raw"\t", '\r' -> raw"\r")

  def escape(scalaString: String): String =
    scalaString.foldLeft(StringBuilder()) { (acc, c) =>
      escapeSeqs.get(c) match {
        case Some(s) => acc ++= s
        case None => acc += c
      }
    }.toString()
}
