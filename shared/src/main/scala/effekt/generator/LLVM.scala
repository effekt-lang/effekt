package effekt.generator

import effekt.context.Context
import effekt.machine
import effekt.llvm._
import effekt.symbols.Module
import effekt.symbols.{ BlockSymbol, Name, Symbol, ValueSymbol }
import org.bitbucket.inkytonik.kiama
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document
import kiama.util.Source
import org.bitbucket.inkytonik.kiama.util.Counter
import effekt.context.assertions._
import effekt.util.GenericPrinter

import scala.language.implicitConversions
import effekt.util.paths._

import scala.sys.process.Process

class LLVM extends Generator {

  /**
   * This is used for both: writing the files to and generating the `require` statements.
   */
  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath

  def llvmPath(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".ll"

  def optPath(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + "_opt.ll"

  def objectPath(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath + ".o"

  /**
   * This is only called on the main entry point, we have to manually traverse the dependencies
   * and write them.
   */
  def run(src: Source)(implicit C: Context): Option[Document] = for {

    mod <- C.frontend(src)
    mainName = C.checkMain(mod)

    // TODO why is backend returning Option? What if compilation fails?
    coreMods = (mod.dependencies :+ mod).flatMap(m => C.backend(m.source))
    // TODO get rid of this object!
    machiner = new machine.Transformer
    // TODO get rid of the module! we only need it for fresh name generation
    llvmDefs = C.using(module = mod) {
      val machineMods = coreMods.map(m => machiner.transform(m)(machiner.TransformerContext(C)))
      machineMods.flatMap(m => LLVMTransformer.transform(m)(LLVMTransformer.LLVMTransformerContext(mod, C)))
    }
    result = LLVMPrinter.wholeProgram(mainName, llvmDefs)(LLVMPrinter.LLVMContext())

    llvmFile = llvmPath(mod)
    _ = C.saveOutput(result.layout, llvmFile)

    optFile = optPath(mod)
    optCommand = Process(Seq("opt-12", llvmFile, "-S", "-O2", "-o", optFile))
    _ = C.config.output().emit(optCommand.!!)

    objectFile = objectPath(mod)
    llcCommand = Process(Seq("llc-12", "--relocation-model=pic", optFile, "-filetype=obj", "-o", objectFile))
    _ = C.config.output().emit(llcCommand.!!)

    mainFile = (C.config.libPath / "main.c").unixPath
    executableFile = path(mod)
    gccCommand = Process(Seq("gcc", mainFile, "-o", executableFile, objectFile))
    _ = C.config.output().emit(gccCommand.!!)

  } yield result

}

object LLVMPrinter extends ParenPrettyPrinter {

  import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

  def wholeProgram(mainName: BlockSymbol, defs: List[Top])(implicit C: LLVMContext): Document =
    pretty(
      vsep(defs.map(toDoc), line) <@@@>
        "define" <+> "void" <+> "@effektMain" <> "()" <+> llvmBlock(
          "%spp = alloca %Sp" <@>
            // TODO find return type of main
            "store %Sp null, %Sp* %spp" <@>
            storeFrm("%spp", "@base", "@limit", "@boxessp", "@boxesbase", "@boxeslimit", List(), globalBuiltin("topLevel"), List(machine.PrimInt())) <@>
            "%newsp = load %Sp, %Sp* %spp" <@>
            // TODO deal with top-level evidence
            jump(globalName(mainName), "%newsp", List("%Evi 0"))
        )
    )

  def toDoc(top: Top)(implicit C: LLVMContext): Doc = top match {
    case DefCnt(functionName, params, entry, body) =>
      // This magical 5 ensures that we pass at most 6 64bit parameters
      val unspilledParams = params.take(5);
      val spilledParams = params.drop(5);
      // TODO spill both kinds of params to non-box stack
      val spilledUnboxedParams = spilledParams.filterNot(p => isBoxType(p.typ));
      val spilledBoxedParams = spilledParams.filter(p => isBoxType(p.typ));
      define(globalName(functionName), unspilledParams.map(toDoc),
        loadSpilled("%spp", "@boxessp", spilledUnboxedParams, spilledBoxedParams) <@>
          "br" <+> "label" <+> localName(entry) <@@@>
          onSeparateLines(body.map(toDoc)))
    case DefFrm(functionName, params, env, entry, body) =>
      define(globalName(functionName), params.map(toDoc),
        loadEnv("%spp", "@boxessp", env) <@>
          "br" <+> "label" <+> localName(entry) <@@@>
          onSeparateLines(body.map(toDoc)))
    case DefClo(functionName, params, env, entry, body) =>
      val emptyStk = freshLocalName("emptyStk");
      define(globalName(functionName), params.map(toDoc),
        loadEnv("%spp", "@boxessp", env) <@>
          emptyStk <+> "=" <+>
          "call fastcc %Stk*" <+> globalBuiltin("popStack") <>
          argumentList(List("%Sp* %spp")) <@>
          "call fastcc void" <+> globalBuiltin("eraseStack") <>
          argumentList(List("%Stk*" <+> emptyStk)) <@>
          "br" <+> "label" <+> localName(entry) <@@@>
          onSeparateLines(body.map(toDoc)))
    case DefFun(returnType, functionName, parameters, body) =>
      "define fastcc" <+> toDoc(returnType) <+> globalName(functionName) <>
        // we can't use unique id here, since we do not know it in the extern string.
        argumentList(parameters.map {
          case machine.Param(typ, id) => toDoc(typ) <+> "%" <> id.name.toString()
        }) <+>
        "alwaysinline" <+> llvmBlock(
          string(body)
        )
    case Include(content) =>
      string(content)
  }

  def toDoc(basicBlock: BasicBlock)(implicit C: LLVMContext): Doc = basicBlock match {
    case BasicBlock(blockName, instructions, terminator) =>
      nameDef(blockName) <> colon <@>
        onLines(instructions.map(toDoc)) <@>
        toDoc(terminator)
  }

  def toDoc(instruction: Instruction)(implicit C: LLVMContext): Doc = instruction match {
    case Call(name, returnType, blockName, args) => {
      localName(name) <+> "=" <+>
        "call fastcc" <+> toDoc(returnType) <+> globalName(blockName) <> argumentList(args.map(toDocWithType))
    }
    case Phi(machine.Param(typ, name), args) => {
      localName(name) <+> "=" <+> "phi" <+> toDoc(typ) <+>
        hsep(args.toList.map {
          case (label, value) =>
            brackets(toDoc(value) <> comma <+> localName(label))
        }, comma)
    }
    case InsertValues(name, typ, args) =>
      insertValues(localName(name), toDoc(typ), args.map(toDocWithType))
    case ExtractValue(name, target, field) =>
      localName(name) <+> "=" <+>
        "extractvalue" <+> toDocWithType(target) <> comma <+> field.toString
    case Inject(name, typ, variant) =>
      localName(name) <+> "=" <+> "add i64 0, " <> variant.toString
    case PushFrame(cntType, blockName, freeVars) =>
      storeFrm("%spp", "@base", "@limit", "@boxessp", "@boxesbase", "@boxeslimit", freeVars, globalName(blockName), cntType)
    case NewStack(cntType, stackName, blockName, args) =>
      val tmpstkp = freshLocalName("tempstkp");
      tmpstkp <+> "=" <+> "call fastcc %Stk*" <+> globalBuiltin("newStack") <>
        argumentList(List()) <@>
        "call fastcc void" <+> globalBuiltin("pushStack") <>
        argumentList(List("%Sp* %spp", "%Stk*" <+> tmpstkp)) <@>
        storeFrm("%spp", "@base", "@limit", "@boxessp", "@boxesbase", "@boxeslimit", args, globalName(blockName), cntType) <@>
        localName(stackName) <+> "=" <+>
        "call fastcc %Stk*" <+> globalBuiltin("popStack") <>
        argumentList(List("%Sp* %spp"))
    case PushStack(stack) =>
      "call fastcc void" <+> globalBuiltin("pushStack") <>
        argumentList(List("%Sp* %spp", toDocWithType(stack)))
    case PopStack(stackName) =>
      localName(stackName) <+> "=" <+>
        "call fastcc %Stk*" <+> globalBuiltin("popStack") <>
        argumentList(List("%Sp* %spp"))
    case CopyStack(stackName, stack) =>
      localName(stackName) <+> "=" <+>
        "call fastcc %Stk*" <+> globalBuiltin("copyStack") <>
        argumentList(List(toDocWithType(stack)))
    case EraseStack(stack) =>
      "call fastcc void" <+> globalBuiltin("eraseStack") <>
        argumentList(List(toDocWithType(stack)))
    case EviPlus(eviName, evi1, evi2) =>
      localName(eviName) <+> "=" <+>
        "add" <+> toDocWithType(evi1) <> comma <+> toDoc(evi2)
    case EviDecr(eviName, evi) =>
      localName(eviName) <+> "=" <+>
        "sub" <+> toDocWithType(evi) <> comma <+> "1"
    case EviIsZero(condName, evi) =>
      localName(condName) <+> "=" <+>
        "icmp eq" <+> toDocWithType(evi) <> comma <+> "0"
  }

  def toDoc(terminator: Terminator)(implicit C: LLVMContext): Doc = terminator match {
    case Ret(values) =>
      // TODO spill arguments to stack (like with jump)
      val newsp = freshLocalName("newsp");
      val cntName = freshLocalName("next");
      load("%spp", cntName, cntTypeDoc(values.map(valueType(_)))) <@>
        newsp <+> "=" <+> "load %Sp, %Sp* %spp" <@>
        jump(cntName, newsp, values.map(toDocWithType(_)))

    case Jump(name, args) =>
      // This magical 5 ensures that we pass at most 6 64bit parameters
      val unspilledArgs = args.take(5);
      val spilledArgs = args.drop(5);
      // TODO spill both kinds of params to non-box stack
      val spilledUnboxedArgs = spilledArgs.filterNot(v => isBoxType(valueType(v)));
      val spilledBoxedArgs = spilledArgs.filter(v => isBoxType(valueType(v)));
      val sp = freshLocalName("sp");
      storeSpilled("%spp", "@base", "@limit", "@boxessp", "@boxesbase", "@boxeslimit", spilledUnboxedArgs, spilledBoxedArgs) <@>
        sp <+> "=" <+> "load %Sp, %Sp* %spp" <@>
        jump(globalName(name), sp, unspilledArgs.map(toDocWithType))
    case JumpLocal(name, args) =>
      "br" <+> "label" <+> localName(name)
    case If(cond, thenBlock, _, elseBlock, _) =>
      "br" <+> toDocWithType(cond) <> comma <+>
        "label" <+> localName(thenBlock) <+> comma <+> "label" <+> localName(elseBlock)
    case Switch(arg, labels) =>
      //ToDo: default label!
      "switch" <+> toDocWithType(arg) <> comma <+> "label" <+> localName(labels.head) <+> brackets(hsep(labels.zipWithIndex.map {
        case (l, i) => "i64" <+> i.toString <> comma <+> "label" <+> localName(l)
      }, " "))
  }

  def toDocWithType(value: machine.Value)(implicit C: LLVMContext): Doc =
    toDoc(valueType(value)) <+> toDoc(value)

  def toDoc(value: machine.Value)(implicit C: LLVMContext): Doc = value match {
    case machine.Var(typ, name) => localName(name)
    case machine.IntLit(n)      => n.toString()
    case machine.BooleanLit(b)  => b.toString()
    case machine.UnitLit()      => 0.toString()
    case machine.EviLit(n)      => n.toString()
  }

  def toDoc(param: machine.Param): Doc = param match {
    case machine.Param(typ, name) => toDoc(typ) <+> localName(name)
  }

  def valueType(value: machine.Value): machine.Type = value match {
    case machine.Var(typ, _)   => typ
    case machine.IntLit(_)     => machine.PrimInt()
    case machine.BooleanLit(_) => machine.PrimBoolean()
    case machine.UnitLit()     => machine.PrimUnit()
    case machine.EviLit(_)     => machine.Evidence()
  }

  def toDoc(typ: machine.Type): Doc =
    typ match {
      case machine.PrimInt()             => "%Int"
      case machine.PrimBoolean()         => "%Boolean"
      case machine.PrimUnit()            => "%Unit"
      case machine.Record(fieldTypes)    => braces(hsep(fieldTypes.map(t => toDoc(t)), comma))
      case machine.Stack(_)              => "%Stk*"
      case machine.Evidence()            => "%Evi"
      case machine.Variant(variantTypes) => "i64"
    }

  // /**
  //  * Auxiliary macros
  //  */

  def define(name: Doc, args: List[Doc], body: Doc): Doc =
    "define fastcc void" <+> name <> argumentList("%Sp noalias %sp" :: args) <+>
      llvmBlock(
        "%spp = alloca %Sp" <@>
          "store %Sp %sp, %Sp* %spp" <@@@>
          body
      )

  def jump(name: Doc, sp: Doc, args: List[Doc])(implicit C: LLVMContext): Doc = {
    "tail call fastcc void" <+> name <> argumentList(("%Sp" <+> sp) :: args) <@>
      "ret" <+> "void"
  }

  def loadEnv(spp: Doc, boxesspp: Doc, params: List[machine.Param])(implicit C: LLVMContext): Doc = {

    val unboxedParams =
      params.filterNot(p => isBoxType(p.typ));
    val unboxedEnvType =
      envRecordType(unboxedParams.map(p => toDoc(p.typ)));
    val unboxedEnvParams =
      unboxedParams.map(p => localName(p.id));
    val boxedParams =
      params.filter(p => isBoxType(p.typ));
    val boxedEnvType =
      envRecordType(boxedParams.map(p => toDoc(p.typ)));
    val boxedEnvParams =
      boxedParams.map(p => localName(p.id));
    (unboxedEnvParams, boxedEnvParams) match {
      case (List(), List()) =>
        emptyDoc
      case (_, List()) =>
        loadParams(spp, unboxedEnvType, unboxedEnvParams)
      case (List(), _) =>
        loadParams(boxesspp, boxedEnvType, boxedEnvParams)
      case (_, _) =>
        loadParams(spp, unboxedEnvType, unboxedEnvParams) <@>
          loadParams(boxesspp, boxedEnvType, boxedEnvParams)
    }
  }

  def storeFrm(spp: Doc, basep: Doc, limitp: Doc, boxesspp: Doc, boxesbasep: Doc, boxeslimitp: Doc, values: List[machine.Value], cntName: Doc, cntType: List[machine.Type])(implicit C: LLVMContext): Doc = {

    val unboxedValues =
      values.filterNot(v => isBoxType(valueType(v)));
    val unboxedEnvType =
      envRecordType(unboxedValues.map(v => toDoc(valueType(v))) :+ cntTypeDoc(cntType));
    val unboxedEnvValues =
      unboxedValues.map(v => toDocWithType(v)) :+ (cntTypeDoc(cntType) <+> cntName);
    val boxedValues =
      values.filter(v => isBoxType(valueType(v)));
    val boxedEnvType =
      envRecordType(boxedValues.map(v => toDoc(valueType(v))));
    val boxedEnvValues =
      boxedValues.map(v => toDocWithType(v));
    (unboxedEnvValues, boxedEnvValues) match {
      case (List(), List()) =>
        emptyDoc
      case (_, List()) =>
        storeValues(spp, basep, limitp, unboxedEnvType, unboxedEnvValues)
      case (List(), _) =>
        storeValues(boxesspp, boxesbasep, boxeslimitp, boxedEnvType, boxedEnvValues)
      case (_, _) =>
        storeValues(spp, basep, limitp, unboxedEnvType, unboxedEnvValues) <@>
          storeValues(boxesspp, boxesbasep, boxeslimitp, boxedEnvType, boxedEnvValues)
    }
  }

  def loadSpilled(spp: Doc, boxesspp: Doc, unboxedParams: List[machine.Param], boxedParams: List[machine.Param])(implicit C: LLVMContext): Doc = {
    val unboxedEnvType =
      envRecordType(unboxedParams.map(p => toDoc(p.typ)));
    val unboxedEnvParams =
      unboxedParams.map(p => localName(p.id));
    val boxedEnvType =
      envRecordType(boxedParams.map(p => toDoc(p.typ)));
    val boxedEnvParams =
      boxedParams.map(p => localName(p.id));
    (unboxedEnvParams, boxedEnvParams) match {
      case (List(), List()) =>
        emptyDoc
      case (_, List()) =>
        loadParams(spp, unboxedEnvType, unboxedEnvParams)
      case (List(), _) =>
        loadParams(boxesspp, boxedEnvType, boxedEnvParams)
      case (_, _) =>
        loadParams(spp, unboxedEnvType, unboxedEnvParams) <@>
          loadParams(boxesspp, boxedEnvType, boxedEnvParams)
    }
  }

  def storeSpilled(spp: Doc, basep: Doc, limitp: Doc, boxesspp: Doc, boxesbasep: Doc, boxeslimitp: Doc, unboxedValues: List[machine.Value], boxedValues: List[machine.Value])(implicit C: LLVMContext): Doc = {
    val unboxedEnvType =
      envRecordType(unboxedValues.map(v => toDoc(valueType(v))));
    val unboxedEnvValues =
      unboxedValues.map(v => toDocWithType(v));
    val boxedEnvType =
      envRecordType(boxedValues.map(v => toDoc(valueType(v))));
    val boxedEnvValues =
      boxedValues.map(v => toDocWithType(v));
    (unboxedEnvValues, boxedEnvValues) match {
      case (List(), List()) =>
        emptyDoc
      case (_, List()) =>
        storeValues(spp, basep, limitp, unboxedEnvType, unboxedEnvValues)
      case (List(), _) =>
        storeValues(boxesspp, boxesbasep, boxeslimitp, boxedEnvType, boxedEnvValues)
      case (_, _) =>
        storeValues(spp, basep, limitp, unboxedEnvType, unboxedEnvValues) <@>
          storeValues(boxesspp, boxesbasep, boxeslimitp, boxedEnvType, boxedEnvValues)
    }
  }

  def loadParams(spp: Doc, envType: Doc, envParams: List[Doc])(implicit C: LLVMContext): Doc = {
    val envName = freshLocalName("env");
    load(spp, envName, envType) <@>
      extractParams(envName, envType, envParams)
  }

  def storeValues(spp: Doc, basep: Doc, limitp: Doc, envType: Doc, envValues: List[Doc])(implicit C: LLVMContext): Doc = {
    val envName = freshLocalName("env");
    insertValues(envName, envType, envValues) <@>
      store(spp, basep, limitp, envName, envType)
  }

  def extractParams(envName: Doc, envType: Doc, envParams: List[Doc]): Doc = {
    onLines {
      envParams.zipWithIndex.map {
        case (p, i) =>
          p <+> "=" <+> "extractvalue" <+> envType <+> envName <> comma <+> i.toString
      }
    }
  }

  def insertValues(envName: Doc, envType: Doc, envValues: List[Doc])(implicit C: LLVMContext): Doc = {

    // TODO this only works when envValues is nonEmpty
    var env = "undef";

    def loop(elements: List[(Doc, Int)]): Doc = elements match {
      case List() => emptyDoc
      case (element, index) :: List() =>
        val oldenv = env;
        envName <+> "=" <+> "insertvalue" <+> envType <+> oldenv <> comma <+>
          element <> comma <+> index.toString
      case (element, index) :: rest =>
        val oldenv = env;
        val newenv = freshLocalName("tmpenv");
        env = newenv;
        newenv <+> "=" <+> "insertvalue" <+> envType <+> oldenv <> comma <+>
          element <> comma <+> index.toString <@>
          loop(rest)
    };
    loop(envValues.zipWithIndex)
  }

  def load(spp: Doc, name: Doc, typ: Doc)(implicit C: LLVMContext): Doc = {

    // TODO name for oldsp and newsp based on name of spp
    val ptrType = typ <> "*"
    val oldsp = freshLocalName("oldsp");
    val newsp = freshLocalName("newsp");
    val oldtypedsp = freshLocalName("oldtypedsp");
    val newtypedsp = freshLocalName("newtypedsp");
    oldsp <+> "=" <+> "load %Sp, %Sp*" <+> spp <@>
      oldtypedsp <+> "=" <+> "bitcast" <+> "%Sp" <+> oldsp <+> "to" <+> ptrType <@>
      newtypedsp <+> "=" <+> "getelementptr" <+> typ <> comma <+> ptrType <+>
      oldtypedsp <> comma <+> "i64 -1" <@>
      newsp <+> "=" <+> "bitcast" <+> ptrType <+> newtypedsp <+> "to" <+> "%Sp" <@>
      name <+> "=" <+> "load" <+> typ <> comma <+> ptrType <+> newtypedsp <@>
      "store %Sp" <+> newsp <> ", %Sp*" <+> spp
  }

  def store(spp: Doc, basep: Doc, limitp: Doc, value: Doc, typ: Doc)(implicit C: LLVMContext): Doc = {

    val ptrType = typ <> "*";
    val oldsp = freshLocalName("oldsp");
    val oldtypedsp = freshLocalName("oldtypedsp");
    val incedtypedsp = freshLocalName("incedtypedsp");
    val incedsp = freshLocalName("incedsp");
    val newsp = freshLocalName("newsp");
    val newtypedsp = freshLocalName("newtypedsp");
    oldsp <+> "=" <+> "load %Sp, %Sp*" <+> spp <@>
      oldtypedsp <+> "=" <+> "bitcast" <+> "%Sp" <+> oldsp <+> "to" <+> ptrType <@>
      incedtypedsp <+> "=" <+> "getelementptr" <+> typ <> comma <+> ptrType <+>
      oldtypedsp <> comma <+> "i64 1" <@>
      incedsp <+> "=" <+> "bitcast" <+> ptrType <+> incedtypedsp <+> "to" <+> "%Sp" <@>
      newsp <+> "=" <+> "call fastcc %Sp" <+> globalBuiltin("checkOverflow") <>
      argumentList(List("%Sp" <+> incedsp, "%Sp*" <+> spp, "%Base*" <+> basep, "%Limit*" <+> limitp)) <@>
      newtypedsp <+> "=" <+> "bitcast" <+> "%Sp" <+> newsp <+> "to" <+> ptrType <@>
      "store" <+> typ <+> value <> comma <+> ptrType <+> newtypedsp
    // TODO do the store to spp here and not in growStack
  }

  def envRecordType(types: List[Doc]): Doc = {
    braces(hsep(types, comma))
  }

  def localName(id: Symbol): Doc =
    "%" <> nameDef(id)

  def globalName(id: Symbol): Doc =
    "@" <> nameDef(id)

  def nameDef(id: Symbol): Doc =
    id.name.toString + "_" + id.id

  def globalBuiltin(name: String): Doc =
    "@" <> name

  def isBoxType(typ: machine.Type) = typ match {
    case machine.Stack(_) => true
    case _                => false
  };

  def cntTypeDoc(cntType: List[machine.Type])(implicit C: LLVMContext): Doc =
    "void" <+> parens(hsep("%Sp" :: cntType.map(toDoc(_)), comma)) <> "*"

  def freshLocalName(name: String)(implicit C: LLVMContext): String =
    "%" + name + "_" + C.fresh.next().toString()

  def llvmBlock(content: Doc): Doc = braces(nest(line <> content) <> line)

  implicit class MyDocOps(self: Doc) {
    def <@@@>(other: Doc): Doc = self <> emptyline <> other
  }

  def argumentList(args: List[Doc]) = parens(hsep(args, comma))

  def onLines(docs: Iterable[Doc]): Doc = docs match {
    case List()  => emptyDoc
    case d :: ds => ds.foldLeft(d)(_ <@> _)
  }

  def onSeparateLines(docs: Iterable[Doc]): Doc = docs match {
    case List()  => emptyDoc
    case d :: ds => ds.foldLeft(d)(_ <@@@> _)
  }

  val emptyline: Doc = line <> line

  /**
   * Extra info in context
   */

  case class LLVMContext() {
    val fresh = new Counter(0)
  }

}
