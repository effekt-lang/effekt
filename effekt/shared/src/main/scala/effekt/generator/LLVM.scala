package effekt.generator

import kiama.output.PrettyPrinterTypes.{ Document, emptyLinks }
import kiama.util.{ Source, Counter }

import effekt.context.Context
import effekt.machine
import effekt.llvm._
import effekt.symbols.Module
import effekt.symbols.{ BlockSymbol, Name, Symbol, ValueSymbol }
import effekt.context.assertions._
import effekt.machine.{ Evidence, PrimBoolean, PrimInt, PrimUnit, Stack, Variant }
import effekt.util.GenericPrinter

import scala.language.implicitConversions
import effekt.util.paths._


// TODO This magical 5 ensures that we pass at most 6 64bit parameters
val MAGICAL_FIVE = 5

// after de-kiama-ing, everything will have been dissolved into strings
type LLVMFragment = String

// indent all but the first line with four spaces
def indentFollowingLines(text: String): String = text.split("\n").map("    " + _).mkString("\n").drop(4)

def commaSeparated(args: List[String]): String = args.mkString(", ")
def spaceSeparated(args: List[String]): String = args.mkString(" ")


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

    val llvm: LLVMFragment = LLVMFragmentPrinter.wholeProgramOneshot(mainName, llvmDefs)
    return Some(Document(llvm, emptyLinks))
  }
}


object LLVMFragmentPrinter {
  case class LLVMContext() {
    val fresh = new Counter(0)
  }

  def wholeProgramOneshot(mainName: BlockSymbol, defs: List[Top]): LLVMFragment =
    implicit val C = LLVMFragmentPrinter.LLVMContext()
    s"""
${defs.map(asFragment).mkString("\n\n")}

define void @effektMain() {
    %spp = alloca %Sp
    ; TODO find return type of main
    store %Sp null, %Sp* %spp
    ${storeFrm("%spp", List(), globalBuiltin("topLevel"), List(machine.PrimInt()))}
    %newsp = load %Sp, %Sp* %spp
    ; TODO deal with top-level evidence
    ${jump(globalName(mainName), "%newsp", List("%Evi 0"))}
}
"""

  def asFragment(top: Top)(implicit C: LLVMContext): LLVMFragment = top match {

    // "DEFine C???NT"
    case DefCnt(functionName, params, entry, body) =>
      val (unspilled,spilled) = params.splitAt(MAGICAL_FIVE)
      s"""
define fastcc void ${globalName(functionName)}(%Sp noalias %sp, ${commaSeparated(unspilled.map(asFragment))}) {
    %spp = alloca %Sp
    store %Sp %sp, %Sp* %spp
    ${loadSpilled("%spp", spilled)}
    br label ${localName(entry)}

    ${indentFollowingLines(body.map(asFragment).mkString("\n\n"))}
}
"""

    // "DEFine FRaMe"
    case DefFrm(functionName, params, env, entry, body) =>
      s"""
define fastcc void ${globalName(functionName)}(%Sp noalias %sp, ${commaSeparated(params.map(asFragment))}) {
    %spp = alloca %Sp
    store %Sp %sp, %Sp* %spp
    ${loadEnv("%spp", env)}
    br label ${localName(entry)}

    ${indentFollowingLines(body.map(asFragment).mkString("\n\n"))}
}
"""

    // "DEFine CLO???"
    case DefClo(functionName, params, env, entry, body) =>
      val emptyStk = freshLocalName("emptyStk")
      s"""
define fastcc void ${globalName(functionName)}(%Sp noalias %sp, ${commaSeparated(params.map(asFragment))}) {
    %spp = alloca %Sp
    store %Sp %sp, %Sp* %spp
    ${loadEnv("%spp", env)}
    $emptyStk = call fastcc %Stk* ${globalBuiltin("popStack")}(%Sp* %spp)
    call fastcc void ${globalBuiltin("eraseStack")}(%Stk* $emptyStk)
    br label ${localName(entry)}

    ${indentFollowingLines(body.map(asFragment).mkString("\n\n"))}
}
"""

    // "DEFine Function"
    case DefFun(returnType, functionName, parameters, body: LLVMFragment) =>
      // we can't use unique id here, since we do not know it in the extern string.
      val params = parameters.map {
        case machine.Param(typ, id) => { /* TODO why is the raw symbol name used here? `assertSaneName(id.name);`*/ s"${asFragment(typ)} %${id.name}" }
      }
      s"""
define fastcc ${asFragment(returnType)} ${globalName(functionName)}(${commaSeparated(params)}) alwaysinline {
    ${indentFollowingLines(body)}
}
"""

    // "DEFine stack SCaNner"
    case DefScn(functionName, env) =>
      // TODO make loadEnv work on sp directly and don't allocate spp
      // TODO do properly (`ret %Sp null` leaks, segfaults and crashes)
      s"""
define fastcc %Sp ${scanningName(functionName)}(%Sp noalias %sp) {
    ret %Sp null ; THIS IS INCORRECT
}
"""
    case Include(raw: String) =>
      raw
  }

  def asFragment(bb: BasicBlock)(implicit C: LLVMContext): LLVMFragment = bb match {
    case BasicBlock(id: BlockSymbol, instructions: List[Instruction], terminator: Terminator) =>
      s"""
${nameDef(id)}:
${instructions.map(asFragment).mkString("\n")}
${asFragment(terminator)}
"""
  }

  def asFragment(instruction: Instruction)(implicit C: LLVMContext): LLVMFragment = instruction match {
    case Call(name, returnType, blockName, args) =>
        s"${localName(name)} = call fastcc ${asFragment(returnType)} ${globalName(blockName)}(${commaSeparated(args.map(fromMachineValueWithAnnotatedType))})"

    case Phi(machine.Param(typ, name), args) =>
      val arg2s = args.toList.map { case (label, value: machine.Value) => s"[${asFragment(value)}, ${localName(label)}]" }
      s"${localName(name)} = phi ${asFragment(typ)} ${commaSeparated(arg2s)}"

    case InsertValues(name, typ, args) =>
      insertAllValues(localName(name), asFragment(typ), args.map(fromMachineValueWithAnnotatedType))

    case ExtractValue(name, target, field) =>
      s"${localName(name)} = extractvalue ${fromMachineValueWithAnnotatedType(target)}, $field"

    case Inject(name, typ, arg, variant) =>
      val tmpCons = freshLocalName("tmpcons")
      val argDocWithType = correspondingType(arg) match {
        case PrimUnit() => fromMachineValueWithAnnotatedType(new machine.UnitLit)
        case _          => fromMachineValueWithAnnotatedType(arg)
      }
      s"""
$tmpCons = insertvalue ${asFragment(typ)} undef, $argDocWithType, ${variant + 1}
${localName(name)} = insertvalue ${asFragment(typ)} $tmpCons, i64 ${variant.toString}, 0
"""

    case PushFrame(cntType, blockName, freeVars) =>
      storeFrm("%spp", freeVars, globalName(blockName), cntType)

    case NewStack(cntType, stackName, blockName, args) =>
      val tmpstkp = freshLocalName("tempstkp")
      s"""
$tmpstkp = call fastcc %Stk* ${globalBuiltin("newStack")}()
call fastcc void ${globalBuiltin("pushStack")}(%Sp* %spp, %Stk* $tmpstkp)
${storeFrm("%spp", args, globalName(blockName), cntType)}
${localName(stackName)} = call fastcc %Stk* ${globalBuiltin("popStack")}(%Sp* %spp)
"""

    // TODO Why is this so assymmetric (`PushStack(stack)` vs. `PopStack(stackName)`)?
    case PushStack(stack) =>
        s"call fastcc void ${globalBuiltin("pushStack")}(%Sp* %spp, ${fromMachineValueWithAnnotatedType(stack)})"
    case PopStack(stackName) =>
        s"${localName(stackName)} = call fastcc %Stk* ${globalBuiltin("popStack")}(%Sp* %spp)"

    case CopyStack(stackName, stack) =>
      s"${localName(stackName)} = call fastcc %Stk* ${globalBuiltin("copyStack")}(${fromMachineValueWithAnnotatedType(stack)})"

    case EraseStack(stack) =>
      s"call fastcc void ${globalBuiltin("eraseStack")}(${fromMachineValueWithAnnotatedType(stack)})"

    case EviPlus(eviName, evi1, evi2: machine.Value) =>
      // TODO [2022-06-21, jfrech] Why is this asymmetric in `evi1` and `evi2`?
      s"${localName(eviName)} = add ${fromMachineValueWithAnnotatedType(evi1)}, ${asFragment(evi2)}"

    case EviDecr(eviName, evi) =>
      s"${localName(eviName)} = sub ${fromMachineValueWithAnnotatedType(evi)}, 1"

    case EviIsZero(condName, evi) =>
      s"${localName(condName)} = icmp eq ${fromMachineValueWithAnnotatedType(evi)}, 0"
  }

  def asFragment(terminator: Terminator)(implicit C: LLVMContext): LLVMFragment = terminator match {
    case Ret(values) =>
      // TODO spill arguments to stack (like with jump)
      val newsp = freshLocalName("newsp")
      val cntName = freshLocalName("next")
      s"""
${load("%spp", cntName, cntTypeDoc(values.map(correspondingType)))}
$newsp = load %Sp, %Sp* %spp
${jump(cntName, newsp, values.map(fromMachineValueWithAnnotatedType))}
"""

    case Jump(name, args) =>
      // This magical 5 ensures that we pass at most 6 64bit parameters
      val unspilledArgs = args.take(5)
      val spilledArgs = args.drop(5)
      val sp = freshLocalName("sp")
      s"""
${storeSpilled("%spp", spilledArgs)}
$sp = load %Sp, %Sp* %spp
${jump(globalName(name), sp, unspilledArgs.map(fromMachineValueWithAnnotatedType))}
"""

    case JumpLocal(name, args) =>
      s"br label ${localName(name)}"

    case If(cond, thenBlock, _, elseBlock, _) =>
      s"br ${fromMachineValueWithAnnotatedType(cond)}, label ${localName(thenBlock)}, label ${localName(elseBlock)}"

    case Switch(arg: machine.Value, default, labels) =>
      s"""switch i64 ${asFragment(arg)}, label ${localName(default)} [${spaceSeparated(labels.map { case (i, l) => s"i64 $i, label ${localName(l)}" })}]"""

    case Panic() =>
      s"call void @exit(i64 1) unreachable"
  }

  def fromMachineValueWithAnnotatedType(value: machine.Value)(implicit C: LLVMContext): LLVMFragment =
    s"${asFragment(correspondingType(value))} ${asFragment(value)}"

  def asFragment(value: machine.Value)(implicit C: LLVMContext): LLVMFragment = value match {
    case machine.Var(typ, name) => s"${localName(name)}"
    case machine.IntLit(n)      => s"$n"
    case machine.BooleanLit(b)  => s"$b"
    case machine.UnitLit()      => "0"
    case machine.EviLit(n)      => s"$n"
  }

  def correspondingType(value: machine.Value): machine.Type = value match {
    case machine.Var(typ, _)   => typ
    case machine.IntLit(_)     => machine.PrimInt()
    case machine.BooleanLit(_) => machine.PrimBoolean()
    case machine.UnitLit()     => machine.PrimUnit()
    case machine.EviLit(_)     => machine.Evidence()
  }
  // TODO [2022-06-21, jfrech] what does this comment mean?
  // TODO essential
  def asFragment(typ: machine.Type): LLVMFragment = typ match {
    case machine.PrimInt()             => "%Int"
    case machine.PrimBoolean()         => "%Boolean"
    case machine.PrimUnit()            => "%Unit"
    case machine.Record(fieldTypes)    => s"{${commaSeparated(fieldTypes.map(asFragment))}}"
    case machine.Stack(_)              => "%Stk*"
    case machine.Evidence()            => "%Evi"
    case machine.Variant(variantTypes) => s"{i64, ${commaSeparated(variantTypes.map(asFragment))}}"
  }

  def asFragment(param: machine.Param): LLVMFragment = param match {
    case machine.Param(typ, name) => s"${asFragment(typ)} ${localName(name)}"
  }


  // TODO Why does `jump` not jump but call?
  // [2022-07-04, jfrech] This is a semantic difference between our interpretation of jumping and LLVM's.
  def jump(name: LLVMFragment, sp: LLVMFragment, args: List[LLVMFragment])(implicit C: LLVMContext): LLVMFragment =
    s"""tail call fastcc void $name(%Sp $sp, ${commaSeparated(args)})
ret void
"""

  // TODO I think, and "env" or "params" is a Frame layout
  def loadEnv(spp: LLVMFragment, params: List[machine.Param])(implicit C: LLVMContext): LLVMFragment = {
    val envType =
      envRecordType(params.map(p => asFragment(p.typ)))
    val envParams =
      params.map(p => localName(p.id))
    loadParams(spp, envType, envParams)
  }

  def storeFrm(spp: LLVMFragment, values: List[machine.Value], cntName: LLVMFragment, cntType: List[machine.Type])(implicit C: LLVMContext): LLVMFragment = {
    val envType =
      envRecordType(values.map(asFragment compose correspondingType) :+ cntTypeDoc(cntType))
    val envValues =
      values.map(v => fromMachineValueWithAnnotatedType(v)) :+ (cntTypeDoc(cntType) +" "+ cntName)
    storeValues(spp, envType, envValues)
  }

  def loadSpilled(spp: LLVMFragment, params: List[machine.Param])(implicit C: LLVMContext): LLVMFragment = {
    params match {
      case Nil =>
        ""
      case _ =>
        val envType = envRecordType(params.map(p => asFragment(p.typ)))
        val envParams = params.map(p => localName(p.id))
        loadParams(spp, envType, envParams)
    }
  }

  def storeSpilled(spp: LLVMFragment, values: List[machine.Value])(implicit C: LLVMContext): LLVMFragment = {
    values match {
      case Nil => ""
      case _ =>
        val envType =
          envRecordType(values.map(asFragment compose correspondingType))
        val envValues =
          values.map(fromMachineValueWithAnnotatedType)
        storeValues(spp, envType, envValues)
    }
  }

  def loadParams(spp: LLVMFragment, envType: LLVMFragment, envParams: List[LLVMFragment])(implicit C: LLVMContext): LLVMFragment =
    val envName = freshLocalName("env")
    s"""
${load(spp, envName, envType)}
${extractParams(envName, envType, envParams)}
"""

  def storeValues(spp: LLVMFragment, envType: LLVMFragment, envValues: List[LLVMFragment])(implicit C: LLVMContext): LLVMFragment =
    val envName = freshLocalName("env")
    s"""
${insertAllValues(envName, envType, envValues)}
${store(spp, envName, envType)}
"""

  def extractParams(envName: LLVMFragment, envType: LLVMFragment, envParams: List[LLVMFragment]): LLVMFragment =
    (envParams.zipWithIndex.map { case (p, idx) => s"$p = extractvalue ${envType} $envName, $idx" }).mkString("\n")

  // insert all given values into a one-deep LLVM record
  def insertAllValues(aggName: LLVMFragment, aggType: LLVMFragment, aggValues: List[LLVMFragment])(implicit C: LLVMContext): LLVMFragment = {
    if (aggValues.length <= 0)
      ???

    var prev = "undef"
    aggValues.zipWithIndex.map((value, idx) => {
        val tmp = if (idx == aggValues.length-1) aggName else freshLocalName(s"agg$idx")
        val ln = s"${tmp} = insertvalue ${aggType} ${prev}, ${value}, $idx"
        prev = tmp
        ln
    }).mkString("\n")
  }

  def load(spp: LLVMFragment, name: LLVMFragment, typ: LLVMFragment)(implicit C: LLVMContext): LLVMFragment =
    val ptrType = typ + "*"
    val oldsp = freshLocalName("oldsp")
    val newsp = freshLocalName("newsp")
    val oldtypedsp = freshLocalName("oldtypedsp")
    val newtypedsp = freshLocalName("newtypedsp")
    s"""
$oldsp = load %Sp, %Sp* ${spp}
$oldtypedsp = bitcast %Sp $oldsp to $ptrType
$newtypedsp = getelementptr ${typ}, $ptrType $oldtypedsp, i64 -1
$newsp = bitcast $ptrType $newtypedsp to %Sp
${name} = load ${typ}, $ptrType $newtypedsp
store %Sp $newsp, %Sp* ${spp}
"""

  def store(spp: LLVMFragment, value: LLVMFragment, typ: LLVMFragment)(implicit C: LLVMContext): LLVMFragment =
    val ptrType = s"${typ}*"
    val oldsp = freshLocalName("oldsp")
    val oldtypedsp = freshLocalName("oldtypedsp")
    val incedtypedsp = freshLocalName("incedtypedsp")
    val incedsp = freshLocalName("incedsp")
    val newsp = freshLocalName("newsp")
    val newtypedsp = freshLocalName("newtypedsp")
    s"""
$oldsp = load %Sp, %Sp* ${spp}
$oldtypedsp = bitcast %Sp $oldsp to $ptrType
$incedtypedsp = getelementptr ${typ}, $ptrType $oldtypedsp, i64 1
$incedsp = bitcast $ptrType $incedtypedsp to %Sp
$newsp =  call fastcc %Sp ${globalBuiltin("checkOverflow")}(%Sp $incedsp, %Sp* ${spp})
$newtypedsp = bitcast %Sp $newsp to $ptrType
store ${typ} ${value}, $ptrType $newtypedsp
; TODO do the store to spp here and not in growStack
"""

  // NOTE: this most likely is reffering to an *LLVM* record type (Effekt ADTs have not yet been implemented)
  def envRecordType(types: List[LLVMFragment]): LLVMFragment =
    "{" + types.mkString(", ") + "}"

  def localName(id: Symbol): LLVMFragment =
    "%" + nameDef(id)

  def globalName(id: Symbol): LLVMFragment =
    "@" + nameDef(id)

  // XXX Major bug potential: `scanningName` can clash with `globalName`.
  def scanningName(id: Symbol): LLVMFragment =
    "@scan_" + nameDef(id)

  def nameDef(id: Symbol): LLVMFragment =
    val name = s"${id.name}_${id.id}"
    assertSaneName(name)
    s"$name"

  def globalBuiltin(name: String): LLVMFragment =
    assertSaneName(name)
    s"@$name"

  def isBoxType(typ: machine.Type) = typ match {
    case machine.Stack(_) => true
    case _                => false
  }

  def cntTypeDoc(cntType: List[machine.Type])(implicit C: LLVMContext): LLVMFragment =
    s"void (%Sp, ${commaSeparated(cntType.map(asFragment))})*"

  def freshLocalName(name: String)(implicit C: LLVMContext): String =
    assertSaneName(name)
    s"%${name}_${C.fresh.next()}"

  def assertSaneName(name: String): Boolean =
    // TODO Why can this not be a raw string literal:`raw"..."`?
    // TODO Unelegant: RegExp has to be recompiled often.
    if (!name.matches("^[a-zA-Z_$][a-zA-Z0-9_$]*$"))
        throw new Error(s"assertSaneName: $name")
    return true
}
