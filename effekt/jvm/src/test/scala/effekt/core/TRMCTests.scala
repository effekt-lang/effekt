package effekt
package core

import effekt.core.BlockType.Function
import effekt.symbols.Capture
import munit.Clue.generate

class TRMCTests extends CoreTests {
  
  case class ImpossibleStateError(message: String) extends RuntimeException(message: String)
  
  val listId = Id("List")
  val consId = Id("Cons")
  val nilId = Id("Nil")
  val headId = Id("head")
  val tailId = Id("tail")
  val TList: ValueType.Data = ValueType.Data(listId, Nil)
  
  //type List {
  // Nil
  // Cons(head: Int, tail: List)
  //}
  val listDecl = Declaration.Data(listId, Nil, List(
    Constructor(nilId, Nil, Nil),
    Constructor(consId, Nil, List(
      Field(headId, Type.TInt),
      Field(tailId, TList)
    ))
  ))
  
  val DC = DeclarationContext(List(listDecl), Nil)

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
  enum TransformContext {
    case Outer(id: Id)
    case Val(id:Id, body: Stmt, next: TransformContext)
  }
  
  enum TailContext {
    case Empty
    case Cons(head: Expr) //TODO: more cases later?
    case Compose(first: TailContext, second: TailContext)
  }
  
  val ctxType: ValueType = ValueType.Var(Id("ctxTpe"))
  
  def split(context: TransformContext): (TailContext, TransformContext) = context match {
    case TransformContext.Outer(id) => (TailContext.Empty, context)
    case TransformContext.Val(id, body, next) => 
      val (isCons, head) = isNextFrameCons(context)
      if (isCons){
        val (init, rest) = split(next)
        (TailContext.Compose(TailContext.Cons(head.get), init), rest)
      }else{
        (TailContext.Empty, context)
      }
        
  }
  
  def isNextFrameCons(context: TransformContext): (Boolean, Option[Expr]) = context match {
    case TransformContext.Outer(id) => (false, None)
    case TransformContext.Val(id, body, next) => body match {
      case Stmt.Return(expr) => expr match {
        case Expr.Make(data, tag, targs, vargs) => 
          if (tag == consId){
            vargs match {
              case ::(head, next) => (true, Some(head))
              case Nil => throw ImpossibleStateError("vargs of Make for Cons cant be empty")
            }
          } else {
            (false, None)
          }
        case _ => (false, None)
      }
      case _ => (false, None)
    }
  }
  
  def transform(input: Stmt, inputfun: Id, outputfun: Id, context: TransformContext) : Stmt = input match {
    case Stmt.Def(id, block, body) => ???
    case Stmt.Let(id, binding, body) => ???
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => ???
    case Stmt.Return(expr) => ???
    case Stmt.Val(id, binding, body) => transform(binding, inputfun, outputfun, TransformContext.Val(id,body,context))
    case Stmt.App(callee, targs, vargs, bargs) => callee match {
      case Block.BlockVar(id, annotatedTpe, annotatedCapt) => 
        if (id == inputfun) {
          annotatedTpe match {
            case Function(tparams, cparams, vparams, bparams, result) => Stmt.App(Block.BlockVar(outputfun, Function(tparams, cparams, vparams.appended(ctxType), bparams, result), annotatedCapt), targs, vargs.appended(reify(context)), bargs) //TODO:capt?
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
  
  def reify(context: TransformContext) : Expr = context match {
    case TransformContext.Outer(id) =>  ValueVar(id, ctxType) //TODO: fix ctxType ?
    case TransformContext.Val(id, body, next) => body match {
      case Stmt.Return(Make(TList,consId,List(Type.TInt,Type.TInt),List(n,ValueVar(id,TList)))) =>
        val composefun: BlockVar = BlockVar(Id("ctx_composeContext"),Function(Nil,Nil,List(ctxType,ctxType),Nil,ctxType),Set.empty)
        val consctx = Expr.MakeContext(TList,consId,Nil,List(n),Nil)
        Expr.PureApp(composefun,Nil,List(reify(next), consctx)) 
      case _ => ???
    }
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
    //Stmt.Return(Expr.ValueVar(n, Type.TInt))

    // f(n)
    val app = Stmt.App(Block.BlockVar(f, fType, fCapt), Nil, List(Expr.ValueVar(n, Type.TInt)), Nil)
    
    // f2(n,ctx)
    val appexpected = Stmt.App(Block.BlockVar(f2, Function(Nil, Nil, List(Type.TInt, ctxType), Nil, Type.TInt), fCapt), Nil, List(Expr.ValueVar(n, Type.TInt),Expr.ValueVar(ctx,ctxType)), Nil)

    val apptransformed = transform(app, f, f2, TransformContext.Outer(ctx))

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
    //val listType = ValueType.Var(Id("List"))
    val fType: BlockType = Function(Nil, Nil, List(Type.TInt), Nil, TList)
    val fCapt: Captures = Set.empty
    val sub: BlockVar = BlockVar(Id("infixSub"),Function(Nil,Nil,List(Type.TInt,Type.TInt),Nil,Type.TInt),Set.empty)
    val composefun: BlockVar = BlockVar(Id("ctx_composeContext"),Function(Nil,Nil,List(ctxType,ctxType),Nil,ctxType),Set.empty)
    //val ctxfun: BlockVar = ???
    
    
    val tmpId = Id("tmp")
    //return Cons(n,tmp)
    val retStmt = Stmt.Return(Make(TList,consId,List(Type.TInt,Type.TInt),List(ValueVar(n,Type.TInt),ValueVar(tmpId,TList))))
    //f(n-1)
    val callStmt = Stmt.App(Block.BlockVar(f, fType, fCapt), Nil, List(Expr.PureApp(sub,List(Type.TInt,Type.TInt),List(ValueVar(n,Type.TInt),Literal(1,Type.TInt)))),Nil)
    //val tmp= f(n-1)
    //return Cons(n,tmp)
    val inputTree = Stmt.Val(tmpId,callStmt,retStmt)
    
    //n-1
    val minusOne = Expr.PureApp(sub,List(Type.TInt,Type.TInt),List(ValueVar(n,Type.TInt),Literal(1,Type.TInt)))

    //Cons(n,[])
    val consctx = Expr.MakeContext(TList,consId,Nil,List(ValueVar(n,Type.TInt)),Nil)
    
    //compose(ctx,Cons(n,[]))
    val composition = Expr.PureApp(composefun,Nil,List(Expr.ValueVar(ctx,ctxType), consctx))
    
    //f2(n-1,compose(ctx, Cons(n,[]))
    val expected = Stmt.App(Block.BlockVar(f2,Function(Nil, Nil, List(Type.TInt, ctxType), Nil, TList),fCapt),Nil,List(minusOne, composition),Nil)
    
    val inputtransformed = transform(inputTree,f,f2,TransformContext.Outer(ctx))

    assertAlphaEquivalentStatements(inputtransformed,expected)
    //assertEquals(inputtransformed,expected)
  }
//  val TFieldName = ValueType.Data(Id("FieldName"), Nil)
//  val fieldString1 = Expr.Literal(tailId.show,Type.TString) //proxy
//  val fieldString2 = Expr.Literal(tailId, TFieldName) //proxy

  
}