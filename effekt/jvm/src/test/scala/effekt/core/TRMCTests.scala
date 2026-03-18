package effekt
package core

import effekt.core.BlockType.Function
import effekt.symbols.Capture

class TRMCTests extends CoreTests {
  
  case class ImpossibleStateError(message: String) extends RuntimeException(message: String)

  val mainSymbol = Id("main")

  def assertTransformsTo(
                          input: String,
                          transformed: String,
                          names: Names = Names(defaultNames + ("main" -> mainSymbol))
                        )(transform: ModuleDecl => ModuleDecl)(using munit.Location) = {
    val moduleHeader =
      """module test
        |
        |""".stripMargin
    val pInput = parse(moduleHeader + input, "input", names)
    val pExpected = parse(moduleHeader + transformed, "expected", names)

    // the parser is not assigning symbols correctly, so we need to run renamer first
    val renamer = TestRenamer(names)
    val renamed = renamer(pInput)

    val obtained = transform(renamed)
    assertAlphaEquivalent(obtained, pExpected, "Not transformed to")
  }

  def transform(input: String, expected: String)(using munit.Location) = {
    assertTransformsTo(input, expected) { tree =>
      tree
    }
  }
  enum transformContext {
    case outer(id: Id)
  }
  def getIdfromCtx(ctx: transformContext): Id = ctx match {
    case transformContext.outer(id) => id
    case _ => Id("not reachable")
  }

  
  val ctxType: ValueType = ValueType.Var(Id("ctxTpe"))
  
  
  
  def transform(input: Stmt, inputfun: Id, outputfun: Id, context: List[transformContext]) : Stmt = input match {
    case Stmt.Def(id, block, body) => ???
    case Stmt.Let(id, binding, body) => ???
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => ???
    case Stmt.Return(expr) => ???
    case Stmt.Val(id, binding, body) => ???
    case Stmt.App(callee, targs, vargs, bargs) => callee match {
      case Block.BlockVar(id, annotatedTpe, annotatedCapt) => 
        if (id == inputfun) {
          annotatedTpe match {
            case Function(tparams, cparams, vparams, bparams, result) => Stmt.App(Block.BlockVar(outputfun, Function(tparams, cparams, vparams.appended(ctxType), bparams, result), annotatedCapt), targs, vargs.appended(ValueVar(getIdfromCtx(context.last), ctxType)), bargs) //TODO:fix ctxType ?, capt?
            case _ => throw ImpossibleStateError("in an App() Statement a Function should be called")
          }
        }else{
          input
        }
      case Block.BlockLit(tparams, cparams, vparams, bparams, body) => ???
      case Block.Unbox(pure) => ???
      case Block.New(impl) => ???
    }
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => ???
    case Stmt.If(cond, thn, els) => ???
    case Stmt.Match(scrutinee, annotatedTpe, clauses, default) => ???
    case Stmt.Region(body) => ???
    case Stmt.Alloc(id, init, region, body) => ???
    case Stmt.Var(ref, init, capture, body) => ???
    case Stmt.Get(id, annotatedTpe, ref, annotatedCapt, body) => ???
    case Stmt.Put(ref, annotatedCapt, value, body) => ???
    case Stmt.Reset(body) => ???
    case Stmt.Shift(prompt, k, body) => ???
    case Stmt.Resume(k, body) => ???
    case Stmt.Hole(annotatedTpe, span) => ???
  }

  test("simple function call") {

    val n: Id = Id("n")
    val f: Id = Id("f")
    val f2: Id = Id("f2")
    val ctx: Id = Id("ctx")
    val fType: BlockType = Function(Nil, Nil, List(Type.TInt), Nil, Type.TInt)
    val fCapt: Captures = Set.empty

    // (n: Int) => n
    val inputTree = BlockLit(Nil, Nil, List(ValueParam(n, Type.TInt)), Nil,
      Stmt.Return(Expr.ValueVar(n, Type.TInt)))

    // return n
    Stmt.Return(Expr.ValueVar(n, Type.TInt))

    // f(n)
    val app = Stmt.App(Block.BlockVar(f, fType, fCapt), Nil, List(Expr.ValueVar(n, Type.TInt)), Nil)
    
    // f2(n,ctx)
    val appexpected = Stmt.App(Block.BlockVar(f2, Function(Nil, Nil, List(Type.TInt, ctxType), Nil, Type.TInt), fCapt), Nil, List(Expr.ValueVar(n, Type.TInt),Expr.ValueVar(ctx,ctxType)), Nil)

    val apptransformed = transform(app,app, f, f2, List(transformContext.outer(ctx)))

//    val input =
//      """def f(n: Int) =
//        |  (f : (Int) => Int @ {})((sub: (Int, Int) => Int @ {})(n:Int, 1))
//        |""".stripMargin
//
//    val expected =
//      """def f2(n: Int, ctx: Context[Int]) =
//        |  (f2 : (Int) => Int @ {})((sub: (Int, Int) => Int @ {})(n:Int, 1), ctx: Context[Int])
//        |""".stripMargin

    //transform(input, expected)
    assertAlphaEquivalentStatements(apptransformed,appexpected)
  }
  
  test("infinite List"){

    val n: Id = Id("n")
    val f: Id = Id("f")
    val f2: Id = Id("f2")
    val ctx: Id = Id("ctx")
    val listType = ValueType.Var(Id("List")
    val fType: BlockType = Function(Nil, Nil, List(Type.TInt), Nil, listType))
    val fCapt: Captures = Set.empty
    val sub: BlockVar = BlockVar(Id("infixSub"),Function(Nil,Nil,List(Type.TInt,Type.TInt),Nil,Type.TInt),Set.empty)
    val ctx: BlockVar = ???
    
    
    //return Cons(n,tmp)
    val retStmt = Stmt.Return(Make(Nil,Id("Cons"),List(Type.TInt,Type.TInt),List(n,Id("tmp"))))
    //f(n-1)
    val callStmt = Stmt.App(Block.BlockVar(f, fType, fCapt), Nil, List(Expr.PureApp(sub,List(Type.TInt,Type.TInt),List(n,1))), Nil)
    //val tmp= f(n-1)
    //return Cons(n,tmp)
    val inputTree = Stmt.Val(Id("tmp"),callStmt,retStmt)
    
    //n-1
    val minusOne = Expr.PureApp(sub,List(Type.TInt,Type.TInt),List(n,1))

    //Cons(n,[])
    val consctx = Expr.PureApp(ctx,???,???)
    
    val expected = Stmt.App(Block.BlockVar(f2,Function(Nil, Nil, List(Type.TInt, ctxType), Nil, listType),fCapt),Nil,List(minusOne,???),Nil)
  }

}