package effekt
import effekt.context.Annotations
import effekt.context.Context
import kiama.util.{Source, StringSource, FileSource}
import effekt.typer.{Typer,Wellformedness}
import effekt.namer.Namer
import effekt.lifted.ModuleDecl
import effekt.util.messages
import effekt.context.IOModuleDB
import effekt.PhaseResult.Typechecked
import scala.collection.mutable

abstract class AbstractTyperTests extends munit.FunSuite {

  import effekt.source.{CallTarget, Def}
  import effekt.source.Def.Declaration

  /** Collects the result of typechecking. In particular, knows the [[Context]]. */
  case class TyperResult(context: Context, result: Typechecked)
  /** A test that first runs the frontend on `input` and then `body`, where the [[TyperResult]] is made available */
  def testTyper(name: String)(src: Source)(body: TyperResult => Unit): Unit = {
    test(name) {
      val compiler = new effekt.Driver {}
      val configs = compiler.createConfig(Seq(
        "--server"
      ))
      configs.verify()
      compiler.context.setup(configs)
      val Frontend = Parser andThen Namer andThen Typer andThen Wellformedness

      given C: Context = compiler.context

      val result = Frontend(src)
      assert(result.isDefined, "Running the frontend succeeds")
      compiler.compileSource(src, configs)
      val ctx = compiler.context
      ctx.using(module = result.get.mod) {
        // run body
        body(TyperResult(ctx, result.get))
      }
    }
  }
  def testTyper(name: String)(input: String)(body: TyperResult => Unit): Unit =
    testTyper(name)(StringSource(input, name))(body)
  def testTyperFile(name: String)(filename: String)(body: TyperResult => Unit): Unit =
    testTyper(name)(FileSource(filename))(body)

  // <editor-fold desc="Finding symbol">
  // This is not a nice solution. It searches the source tree recursively for
  // symbols with the given name, to make writing tests easier (and allow
  // testing the types of local variables).
  def findSymbolIn(name: String, d: source.Param, acc: mutable.Set[symbols.Symbol])(using R: TyperResult): Unit = {
    given Context = R.context
    if(d.id.name == name) { acc.addOne(d.id.symbol) }
  }
  def findSymbolIn(name: String, id: source.Id, acc: mutable.Set[symbols.Symbol])(using R: TyperResult): Unit = {
    given Context = R.context
    if(id.name == name) { acc.addOne(id.symbol) }
  }

  def findSymbolIn(name: String, t: source.Term, acc: mutable.Set[symbols.Symbol])(using R: TyperResult): Unit = {
    import source.Term
    given Context = R.context
    t match {
      case Term.Var(id) => findSymbolIn(name, id, acc)
      case Term.Assign(id, expr) => acc.addOne(id.symbol); findSymbolIn(name, expr, acc)
      case Term.Select(receiver, id) => findSymbolIn(name, receiver, acc); findSymbolIn(name, id, acc)
      case Term.Do(effect, id, targs, vargs) =>
        findSymbolIn(name, id, acc);
        vargs.foreach(findSymbolIn(name, _, acc))
      case Term.MethodCall(receiver, id, targs, vargs, bargs) =>
        findSymbolIn(name, receiver, acc);
        findSymbolIn(name, id, acc);
        vargs.foreach(findSymbolIn(name,_,acc));
        bargs.foreach(findSymbolIn(name,_,acc))
      case Term.Region(id, body) =>
        findSymbolIn(name, id, acc);
        findSymbolIn(name, body, acc)
      case Term.BlockLiteral(tparams, vparams, bparams, body) =>
        vparams.foreach(findSymbolIn(name,_,acc));
        bparams.foreach(findSymbolIn(name,_,acc));
        findSymbolIn(name,body,acc)
      case Term.Hole(stmts) => findSymbolIn(name,stmts,acc)
      case Term.Box(capt, b) => findSymbolIn(name,b,acc)
      case Term.Unbox(t) => findSymbolIn(name,t,acc)
      case Term.Call(f,tas,vas,bas) =>
        findSymbolIn(name,f,acc);
        vas.foreach(findSymbolIn(name,_,acc));
        bas.foreach(findSymbolIn(name,_,acc))
      case Term.If(c,t,e) =>
        findSymbolIn(name,c,acc);
        findSymbolIn(name,t,acc);
        findSymbolIn(name,e,acc)
      case Term.While(c,b) =>
        findSymbolIn(name,c,acc);
        findSymbolIn(name,b,acc)
      case Term.Match(scr,cls) =>
        findSymbolIn(name,scr,acc);
        cls.foreach{
          case source.MatchClause(pattern, body) =>
            findSymbolIn(name,pattern,acc);
            findSymbolIn(name,body,acc)
        }
      case Term.TryHandle(p,hs) =>
        findSymbolIn(name,p,acc);
        hs.foreach(findSymbolIn(name,_,acc))
      case Term.New(impl) => findSymbolIn(name,impl,acc)
      case _ => ()
    }
  }
  def findSymbolIn(name: String, target: source.CallTarget, acc: mutable.Set[symbols.Symbol])(using R: TyperResult): Unit = {
    target match {
      case CallTarget.IdTarget(id) => findSymbolIn(name,id,acc)
      case CallTarget.ExprTarget(rcv) => findSymbolIn(name,rcv,acc)
    }
  }
  def findSymbolIn(name: String, pattern: source.MatchPattern, acc: mutable.Set[symbols.Symbol])(using R: TyperResult): Unit = {
    import effekt.source.MatchPattern
    pattern match {
      case MatchPattern.AnyPattern(id) => findSymbolIn(name, id, acc)
      case MatchPattern.TagPattern(id, patterns) =>
        patterns.foreach(findSymbolIn(name, _, acc))
      case _ => ()
    }
  }
  def findSymbolIn(name: String, handler: source.Handler, acc: mutable.Set[symbols.Symbol])(using R: TyperResult): Unit = {
    findSymbolIn(name, handler.impl, acc);
    handler.capability.foreach(findSymbolIn(name,_,acc))
  }

  def findSymbolIn(name: String, d: source.Stmt, acc: mutable.Set[symbols.Symbol])(using R: TyperResult): Unit = {
    import source.Stmt
    given Context = R.context
    d match {
      case Stmt.DefStmt(d, rest) => findSymbolIn(name, d, acc); findSymbolIn(name, rest, acc)
      case Stmt.ExprStmt(d, rest) => findSymbolIn(name,d,acc); findSymbolIn(name, rest, acc)
      case Stmt.Return(d) => findSymbolIn(name,d,acc)
      case Stmt.BlockStmt(stmt) => findSymbolIn(name,stmt,acc)
    }
  }

  def findSymbolIn(name: String, d: source.Def, acc: mutable.Set[symbols.Symbol])(using R: TyperResult): Unit = {
      given Context = R.context
      findSymbolIn(name,d.id,acc)
      d match {
        case source.FunDef(id, tps, vps, bps, ret, body) =>
          vps.foreach(findSymbolIn(name, _, acc))
          bps.foreach(findSymbolIn(name, _, acc))
          findSymbolIn(name, body, acc)
        case source.ValDef(id, _, binding) =>
          findSymbolIn(name,binding,acc)
        case source.DefDef(id, annot, binding) =>
          findSymbolIn(name,binding,acc)
        case source.VarDef(id, _, reg, binding) =>
          reg.foreach(findSymbolIn(name,_,acc));
          findSymbolIn(name,binding,acc)
        case source.InterfaceDef(id, tparamsInterface, ops, isEffect) =>
          ops.foreach{ o =>
            findSymbolIn(name,o.id,acc);
            o.params.foreach(findSymbolIn(name,_,acc))
          }
        case source.ExternDef(pure, id, _, vps, bps, _, body) =>
          vps.foreach(findSymbolIn(name,_,acc))
          bps.foreach(findSymbolIn(name,_,acc))
        case _ => ()
      }
  }

  def findSymbolIn(name: String, implementation: source.Implementation, acc: mutable.Set[symbols.Symbol])(using R: TyperResult): Unit = {
    findSymbolIn(name, implementation.id, acc);
    implementation.clauses.foreach{
      case source.OpClause(id, tparams, vparams, ret, body, resume) =>
        findSymbolIn(name,id,acc);
        vparams.foreach(findSymbolIn(name,_,acc));
        findSymbolIn(name,body,acc);
        findSymbolIn(name,resume,acc)
    }
  }

  extension(R: TyperResult) {
    private def findSymbol(name: String): symbols.TermSymbol = {
      given TyperResult = R
      val syms = R.context.module.terms.getOrElse(name, Nil)
      if (syms.isEmpty) {
        val allFound = new mutable.HashSet[symbols.Symbol]()
        R.result.tree.defs.foreach { d => findSymbolIn(name, d, allFound) }
        val found = allFound.collect {
          case s: symbols.TermSymbol => s
        }
        if (found.size == 1) {
          found.head
        } else if (found.isEmpty) {
          fail(s"No symbol with name '${name}' found.")
        } else {
          fail(s"Name ${name} is ambiguous and could refer to multiple symbols")
        }
      } else {
        assert(syms.size == 1, s"There is a unique top-level symbol named '${name}'.")
        syms.head
      }
    }
    // </editor-fold>
  }
  extension(R: TyperResult) {
    /** Assert that the unique symbol named `name` has the expected value type */
    def assertValueType(name: String, expected: String, clue: => Any = "value types don't match"): Unit = {
      val sym = R.findSymbol(name)
      assert(sym.isInstanceOf[symbols.ValueSymbol], s"${sym} is a value symbol.")
      val got = R.context.annotation(Annotations.ValueType, sym.asInstanceOf[symbols.ValueSymbol])
      assertNoDiff(symbols.TypePrinter.show(got), expected, clue)
    }
    /** Assert that the unique symbol named `name` has the expected block type */
    def assertBlockType(name: String, expected: String, clue: => Any = "block types don't match"): Unit = {
      val sym = R.findSymbol(name)
      assert(sym.isInstanceOf[symbols.BlockSymbol], s"${sym} is a block symbol.")
      val got = R.context.annotation(Annotations.BlockType, sym.asInstanceOf[symbols.BlockSymbol])
      assertNoDiff(symbols.TypePrinter.show(got), expected, clue)
    }

    // TODO further assertions (e.g. for captures etc) on the context
  }

}
class TyperTests extends AbstractTyperTests {

  testTyper("Simple example test")(
    """type A{}
      |def foo(x: A) = x
      |""".stripMargin
  ){ C =>
    C.assertBlockType("foo", "A => A")
  }

  testTyper("Simple example test for local variable")(
    """type A{}
      |def foo(x: A) = {
      |  val y = x
      |  12
      |}
      |""".stripMargin
  ){ C =>
    C.assertValueType("y", "A")
  }

}
