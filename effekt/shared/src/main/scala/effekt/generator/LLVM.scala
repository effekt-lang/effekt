package effekt.generator

import effekt.context.Context
import effekt.machine
import effekt.llvm._
import effekt.symbols.Module
import effekt.symbols.{ BlockSymbol, Name, Symbol, ValueSymbol }
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document
import kiama.output.PrettyPrinterTypes.emptyLinks
import kiama.util.Source
import kiama.util.Counter
import effekt.context.assertions._
import effekt.machine.{ Evidence, PrimBoolean, PrimInt, PrimUnit, Stack, Variant }
import effekt.util.GenericPrinter

import scala.language.implicitConversions
import effekt.util.paths._

import scala.sys.process.Process


// XXX `val` to make it publicly accessible
class LLVMSource(val raw: String) {}

class LLVMFragment(val raw: String) {}



class LLVM extends Generator {
  def path(m: Module)(implicit C: Context): String =
    (C.config.outputPath() / m.path.replace('/', '_')).unixPath

  /**
   * This is only called on the main entry point, we have to manually traverse the dependencies
   * and write them.
   */
  //TODO-LLVM refactor
  def run(src: Source)(implicit C: Context): Option[Document] = {
    val modQ = C.frontend(src)
    if (modQ.isEmpty)
      return None
    val mod = modQ.get

    val mainName = C.checkMain(mod)

    // TODO why is backend returning Option? What if compilation fails?
    val coreMods = (mod.dependencies :+ mod).flatMap(m => C.backend(m.source))
    // TODO get rid of this object!
    val machiner = new machine.Transformer
    // TODO get rid of the module! we only need it for fresh name generation
    val llvmDefs = C.using(module = mod) {
      val machineMods = coreMods.map(m => machiner.transform(m)(machiner.TransformerContext(C)))
      machineMods.flatMap(m => LLVMTransformer.transform(m)(LLVMTransformer.LLVMTransformerContext(mod, C)))
    }

    return Some(Document(LLVMPrinter.wholeProgram(mainName, llvmDefs)(LLVMPrinter.LLVMContext()), emptyLinks))
  }
}

object LLVMPrinter extends ParenPrettyPrinter {
  // a transitional helper to aid in moving to string interpolation for LLVM code construction
  def d2s(doc: Doc): String = pretty(doc).layout
  def f2d(frg: LLVMFragment): Doc = Doc(frg.raw)
  def transitionalDocLift(src: LLVMSource): Doc = Doc(src.raw)

  def wholeProgram(mainName: BlockSymbol, defs: List[Top])(implicit C: LLVMContext): String =
    d2s(
      vsep(defs.map(toDoc), line) <@@@> s"""
define void @effektMain() {
    %spp = alloca %Sp
    ; TODO find return type of main
    store %Sp null, %Sp* %spp
    ${d2s(storeFrm("%spp", "@base", "@limit", List(), f2d(globalBuiltin("topLevel")), List(machine.PrimInt())))}
    %newsp = load %Sp, %Sp* %spp
    ; TODO deal with top-level evidence
    ${d2s(jump(globalName(mainName), "%newsp", List("%Evi 0")))}
}
""")

  def toDoc(top: Top)(implicit C: LLVMContext): Doc = top match {

    // "DEFine C???NT"
    case DefCnt(functionName, params, entry, body) =>
      // This magical 5 ensures that we pass at most 6 64bit parameters
      val unspilledParams = params.take(5);
      val spilledParams = params.drop(5);
      define(globalName(functionName), unspilledParams.map(toDoc),
        loadSpilled("%spp", spilledParams) <@>
          "br" <+> "label" <+> localName(entry) <@@@>
          onSeparateLines(body.map(toDoc)))

    // "DEFine FRaMe"
    case DefFrm(functionName, params, env, entry, body) =>
      define(globalName(functionName), params.map(toDoc),
        loadEnv("%spp", env) <@>
          "br" <+> "label" <+> localName(entry) <@@@>
          onSeparateLines(body.map(toDoc)))

    // "DEFine CLO???"
    case DefClo(functionName, params, env, entry, body) =>
      val emptyStk = freshLocalName("emptyStk");
      define(globalName(functionName), params.map(toDoc),
        loadEnv("%spp", env) <@>
          emptyStk <+> "=" <+>
          "call fastcc %Stk*" <+> f2d(globalBuiltin("popStack")) <>
          argumentList(List("%Sp* %spp")) <@>
          "call fastcc void" <+> f2d(globalBuiltin("eraseStack")) <>
          argumentList(List("%Stk*" <+> emptyStk)) <@>
          "br" <+> "label" <+> localName(entry) <@@@>
          onSeparateLines(body.map(toDoc)))

    // "DEFine Function"
    case DefFun(returnType, functionName, parameters, body) =>
      "define fastcc" <+> toDoc(returnType) <+> globalName(functionName) <>
        // we can't use unique id here, since we do not know it in the extern string.
        argumentList(parameters.map {
          case machine.Param(typ, id) => toDoc(typ) <+> "%" <> id.name.toString()
        }) <+>
        "alwaysinline" <+> llvmBlock(
          string(body)
        )

    // "DEFine stack SCaNner"
    case DefScn(functionName, env) =>
      // TODO make loadEnv work on sp directly and don't allocate spp
      "define fastcc %Sp" <+> scanningName(functionName) <> argumentList(List("%Sp noalias %sp")) <+> llvmBlock(
        "ret %Sp null" // TODO do properly (this leaks, segfaults and crashes)
      )
    case Include(content) =>
      string(content)
  }

  def toDoc(basicBlock: BasicBlock)(implicit C: LLVMContext): Doc = basicBlock match {
    case BasicBlock(blockName, instructions, terminator) =>
      f2d(nameDef(blockName)) <> colon <@>
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
    case Inject(name, typ, arg, variant) =>
      val tmpCons = freshLocalName("tmpcons")
      val argDocWithType = valueType(arg) match {
        case PrimUnit() => toDocWithType(new machine.UnitLit)
        case _          => toDocWithType(arg)
      }
      tmpCons <+> "=" <+> "insertvalue" <+> toDoc(typ) <+> "undef," <+> argDocWithType <> "," <+> (variant + 1).toString <@>
        localName(name) <+> "=" <+> "insertvalue" <+> toDoc(typ) <+> tmpCons <> ", i64" <+> variant.toString <> ", 0"
    case PushFrame(cntType, blockName, freeVars) =>
      storeFrm("%spp", "@base", "@limit", freeVars, globalName(blockName), cntType)
    case NewStack(cntType, stackName, blockName, args) =>
      val tmpstkp = freshLocalName("tempstkp");
      tmpstkp <+> "=" <+> "call fastcc %Stk*" <+> f2d(globalBuiltin("newStack")) <>
        argumentList(List()) <@>
        "call fastcc void" <+> f2d(globalBuiltin("pushStack")) <>
        argumentList(List("%Sp* %spp", "%Stk*" <+> tmpstkp)) <@>
        storeFrm("%spp", "@base", "@limit", args, globalName(blockName), cntType) <@>
        localName(stackName) <+> "=" <+>
        "call fastcc %Stk*" <+> f2d(globalBuiltin("popStack")) <>
        argumentList(List("%Sp* %spp"))
    case PushStack(stack) =>
      "call fastcc void" <+> f2d(globalBuiltin("pushStack")) <>
        argumentList(List("%Sp* %spp", toDocWithType(stack)))
    case PopStack(stackName) =>
      localName(stackName) <+> "=" <+>
        "call fastcc %Stk*" <+> f2d(globalBuiltin("popStack")) <>
        argumentList(List("%Sp* %spp"))
    case CopyStack(stackName, stack) =>
      localName(stackName) <+> "=" <+>
        "call fastcc %Stk*" <+> f2d(globalBuiltin("copyStack")) <>
        argumentList(List(toDocWithType(stack)))
    case EraseStack(stack) =>
      "call fastcc void" <+> f2d(globalBuiltin("eraseStack")) <>
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
      transitionalDocLift(load("%spp", cntName, cntTypeDoc(values.map(valueType(_))))) <@>
        newsp <+> "=" <+> "load %Sp, %Sp* %spp" <@>
        jump(cntName, newsp, values.map(toDocWithType(_)))

    case Jump(name, args) =>
      // This magical 5 ensures that we pass at most 6 64bit parameters
      val unspilledArgs = args.take(5);
      val spilledArgs = args.drop(5);
      val sp = freshLocalName("sp");
      storeSpilled("%spp", "@base", "@limit", spilledArgs) <@>
        sp <+> "=" <+> "load %Sp, %Sp* %spp" <@>
        jump(globalName(name), sp, unspilledArgs.map(toDocWithType))
    case JumpLocal(name, args) =>
      "br" <+> "label" <+> localName(name)
    case If(cond, thenBlock, _, elseBlock, _) =>
      "br" <+> toDocWithType(cond) <> comma <+>
        "label" <+> localName(thenBlock) <+> comma <+> "label" <+> localName(elseBlock)
    case Switch(arg, default, labels) =>
      "switch" <+> "i64" <+> toDoc(arg) <> comma <+> "label" <+> localName(default) <+> brackets(hsep(labels.map {
        case (i, l) => "i64" <+> i.toString <> comma <+> "label" <+> localName(l)
      }, " "))
    case Panic() =>
      "call void @exit(i64 1)" <@> "unreachable"
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

  // TODO essential
  def toDoc(typ: machine.Type): Doc =
    typ match {
      case machine.PrimInt()             => "%Int"
      case machine.PrimBoolean()         => "%Boolean"
      case machine.PrimUnit()            => "%Unit"
      case machine.Record(fieldTypes)    => braces(hsep(fieldTypes.map(t => toDoc(t)), comma))
      case machine.Stack(_)              => "%Stk*"
      case machine.Evidence()            => "%Evi"
      case machine.Variant(variantTypes) => braces("i64," <+> hsep(variantTypes.map(t => toDoc(t)), comma))
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

  // TODO Why does `jump` not jump but call?
  def jump(name: Doc, sp: Doc, args: List[Doc])(implicit C: LLVMContext): Doc = {
    "tail call fastcc void" <+> name <> argumentList(("%Sp" <+> sp) :: args) <@>
      "ret" <+> "void"
  }

  // TODO I think, and "env" or "params" is a Frame layout
  def loadEnv(spp: Doc, params: List[machine.Param])(implicit C: LLVMContext): Doc = {
    val envType =
      envRecordType(params.map(p => toDoc(p.typ)));
    val envParams =
      params.map(p => localName(p.id));
    loadParams(d2s(spp), envType, envParams)
  }

  def storeFrm(spp: Doc, basep: Doc, limitp: Doc, values: List[machine.Value], cntName: Doc, cntType: List[machine.Type])(implicit C: LLVMContext): Doc = {
    val envType =
      envRecordType(values.map(v => toDoc(valueType(v))) :+ cntTypeDoc(cntType));
    val envValues =
      values.map(v => toDocWithType(v)) :+ (cntTypeDoc(cntType) <+> cntName);
    storeValues(spp, basep, limitp, envType, envValues)
  }

  def loadSpilled(spp: Doc, params: List[machine.Param])(implicit C: LLVMContext): Doc = {
    params match {
      case Nil => emptyDoc
      case _ =>
        val envType =
          envRecordType(params.map(p => toDoc(p.typ)));
        val envParams =
          params.map(p => localName(p.id));
        loadParams(d2s(spp), envType, envParams)
    }
  }

  def storeSpilled(spp: Doc, basep: Doc, limitp: Doc, values: List[machine.Value])(implicit C: LLVMContext): Doc = {
    values match {
      case Nil => emptyDoc
      case _ =>
        val envType =
          envRecordType(values.map(v => toDoc(valueType(v))));
        val envValues =
          values.map(v => toDocWithType(v));
        storeValues(spp, basep, limitp, envType, envValues)
    }
  }

  def loadParams(spp: String, envType: Doc, envParams: List[Doc])(implicit C: LLVMContext): Doc = {
    val envName = freshLocalName("env");
    transitionalDocLift(load(spp, envName, envType)) <@>
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

  def load(spp: String, name: String, typ: Doc)(implicit C: LLVMContext): LLVMSource = {
    val ptrType = d2s(typ) + "*"
    val oldsp = freshLocalName("oldsp");
    val newsp = freshLocalName("newsp");
    val oldtypedsp = freshLocalName("oldtypedsp");
    val newtypedsp = freshLocalName("newtypedsp");
    LLVMSource(s"""
$oldsp = load %Sp, %Sp* ${d2s(spp)}
$oldtypedsp = bitcast %Sp $oldsp to $ptrType
$newtypedsp = getelementptr ${d2s(typ)}, $ptrType $oldtypedsp, i64 -1
$newsp = bitcast $ptrType $newtypedsp to %Sp
${d2s(name)} = load ${d2s(typ)}, $ptrType $newtypedsp
store %Sp $newsp, %Sp* ${d2s(spp)}
""")
  }

  def store(spp: Doc, basep: Doc, limitp: Doc, value: Doc, typ: Doc)(implicit C: LLVMContext): Doc = {
    // TODO remove parameters spp basep and limitp
    val ptrType = s"${d2s(typ)}*"
    val oldsp = freshLocalName("oldsp")
    val oldtypedsp = freshLocalName("oldtypedsp")
    val incedtypedsp = freshLocalName("incedtypedsp")
    val incedsp = freshLocalName("incedsp")
    val newsp = freshLocalName("newsp")
    val newtypedsp = freshLocalName("newtypedsp")
    d2s(s"""
$oldsp = load %Sp, %Sp* ${d2s(spp)}
$oldtypedsp = bitcast %Sp $oldsp to $ptrType
$incedtypedsp = getelementptr ${d2s(typ)}, $ptrType $oldtypedsp, i64 1
$incedsp = bitcast $ptrType $incedtypedsp to %Sp
$newsp =  call fastcc %Sp ${d2s(f2d(globalBuiltin("checkOverflow")))}${d2s(argumentList(List(s"%Sp $incedsp", s"%Sp* ${d2s(spp)}")))}
$newtypedsp = bitcast %Sp $newsp to $ptrType
store ${d2s(typ)} ${d2s(value)}, $ptrType $newtypedsp
; TODO do the store to spp here and not in growStack
""")
  }

  def envRecordType(types: List[Doc]): Doc = {
    braces(hsep(types, comma))
  }

  def localName(id: Symbol): Doc =
    "%" <> f2d(nameDef(id))

  def globalName(id: Symbol): Doc =
    "@" <> f2d(nameDef(id))

  // XXX Major bug potential: `scanningName` can clash with `globalName`.
  def scanningName(id: Symbol): Doc =
    "@scan_" <> f2d(nameDef(id))

  def nameDef(id: Symbol): LLVMFragment =
    val name = s"${id.name}_${id.id}"
    assertSaneName(name)
    LLVMFragment(name)

  def globalBuiltin(name: String): LLVMFragment =
    assertSaneName(name)
    LLVMFragment(s"@$name")

  def isBoxType(typ: machine.Type) = typ match {
    case machine.Stack(_) => true
    case _                => false
  };

  def cntTypeDoc(cntType: List[machine.Type])(implicit C: LLVMContext): Doc =
    "void" <+> parens(hsep("%Sp" :: cntType.map(toDoc(_)), comma)) <> "*"

  def freshLocalName(name: String)(implicit C: LLVMContext): String =
    assertSaneName(name)
    s"%${name}_${C.fresh.next()}"

  def assertSaneName(name: String): Boolean =
    // TODO Why can this not be a raw string literal:`raw"..."`?
    // TODO Unelegant: RegExp has to be recompiled often.
    if (!name.matches("^[a-zA-Z_$][a-zA-Z0-9_$]*$"))
        throw new Error(s"assertSaneName: $name")
    return true

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
