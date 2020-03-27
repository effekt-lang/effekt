package effekt.symbols

/**
 * The symbols, which are built into Effekt
 */
object builtins {

  private def name(s: String) = QualifiedName("effekt", s)

  val TInt = BuiltinType(name("Int"), Nil)
  val TBoolean = BuiltinType(name("Boolean"), Nil)
  val TUnit = BuiltinType(name("Unit"), Nil)
  val TString = BuiltinType(name("String"), Nil)
  val TDouble = BuiltinType(name("Double"), Nil)
  val TVarR = TypeVar(name("R"))

  val EConsole = BuiltinEffect(name("Console"))
  val EDivZero = BuiltinEffect(name("DivZero"))

  private val twoInts = List(List(ValueParam(LocalName("lhs"), Some(TInt)), ValueParam(LocalName("rhs"), Some(TInt))))
  private val twoBool = List(List(ValueParam(LocalName("lhs"), Some(TBoolean)), ValueParam(LocalName("rhs"), Some(TBoolean))))
  private val twoStrings = List(List(ValueParam(LocalName("lhs"), Some(TString)), ValueParam(LocalName("rhs"), Some(TString))))
  private val twoDoubles = List(List(ValueParam(LocalName("lhs"), Some(TDouble)), ValueParam(LocalName("rhs"), Some(TDouble))))
  private val twoRs = List(List(ValueParam(LocalName("lhs"), Some(TVarR)), ValueParam(LocalName("rhs"), Some(TVarR))))

  val infixAdd = BuiltinFunction(name("addInt"), Nil, twoInts, Some(Effectful(TInt, Pure)))
  val infixSub = BuiltinFunction(name("subInt"), Nil, twoInts, Some(Effectful(TInt, Pure)))
  val infixMul = BuiltinFunction(name("mulInt"), Nil, twoInts, Some(Effectful(TInt, Pure)))
  val infixDiv = BuiltinFunction(name("divInt"), Nil, twoInts, Some(Effectful(TInt, Effects(List(EDivZero)))))
  val mod = BuiltinFunction(name("mod"), Nil, twoInts, Some(Effectful(TInt, Pure)))
  val rand = BuiltinFunction(name("random"), Nil, List(Nil), Some(Effectful(TInt, Pure)))

  val addDouble = BuiltinFunction(name("addDouble"), Nil, twoDoubles, Some(Effectful(TDouble, Pure)))
  val mulDouble = BuiltinFunction(name("mulDouble"), Nil, twoDoubles, Some(Effectful(TDouble, Pure)))
  val subDouble = BuiltinFunction(name("subDouble"), Nil, twoDoubles, Some(Effectful(TDouble, Pure)))

  val infixEq = BuiltinFunction(name("equals"), List(TVarR), twoRs, Some(Effectful(TBoolean, Pure)))
  val infixLte = BuiltinFunction(name("lte"), Nil, twoInts, Some(Effectful(TBoolean, Pure)))
  val infixGte = BuiltinFunction(name("gte"), Nil, twoInts, Some(Effectful(TBoolean, Pure)))
  val infixLt = BuiltinFunction(name("lt"), Nil, twoInts, Some(Effectful(TBoolean, Pure)))
  val infixGt = BuiltinFunction(name("gt"), Nil, twoInts, Some(Effectful(TBoolean, Pure)))
  val infixOr = BuiltinFunction(name("or"), Nil, twoBool, Some(Effectful(TBoolean, Pure)))
  val infixAnd = BuiltinFunction(name("and"), Nil, twoBool, Some(Effectful(TBoolean, Pure)))
  val infixConcat = BuiltinFunction(name("concat"), Nil, twoStrings, Some(Effectful(TString, Pure)))
  val not = BuiltinFunction(name("not"), Nil, List(List(ValueParam(LocalName("b"), Some(TBoolean)))), Some(Effectful(TBoolean, Pure)))
  val printInt = BuiltinFunction(name("println"), List(TVarR), List(List(ValueParam(LocalName("msg"), Some(TVarR)))), Some(Effectful(TUnit, Effects(List(EConsole)))))
  val show = BuiltinFunction(name("show"), List(TVarR), List(List(ValueParam(LocalName("obj"), Some(TVarR)))), Some(Effectful(TString, Pure)))

  val rootTypes: Map[String, TypeSymbol] = Map(
    "Int" -> TInt,
    "Boolean" -> TBoolean,
    "Unit" -> TUnit,
    "String" -> TString,
    "Double" -> TDouble,
    "Console" -> EConsole,
    "DivZero" -> EDivZero
  )

  val rootTerms: Map[String, BuiltinFunction] = Map(
    "infix_+" -> infixAdd,
    "infix_-" -> infixSub,
    "infix_*" -> infixMul,
    "infix_/" -> infixDiv,
    "mod" -> mod,
    "random" -> rand,

    "addDouble" -> addDouble,
    "mulDouble" -> mulDouble,
    "subDouble" -> subDouble,

    "infix_==" -> infixEq,
    "infix_<=" -> infixLte,
    "infix_>=" -> infixGte,
    "infix_<" -> infixLt,
    "infix_>" -> infixGt,
    "not" -> not,

    "infix_++" -> infixConcat,

    // for now those are considered eager
    "infix_||" -> infixOr,
    "infix_&&" -> infixAnd,

    "println" -> printInt,
    "show" -> show
  )

  val rootEffects: Effects = Effects(List(EDivZero, EConsole))
}
