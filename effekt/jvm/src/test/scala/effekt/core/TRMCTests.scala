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
    case Outer(id: Id)
    case Make(data: ValueType.Data, tag: Id, targs: List[ValueType], before: List[Expr], after: List[Expr])
    case Compose(first: TailContext, second: TailContext)
  }
  
  def HoleContext = Type.TInt //(a: ValueType, b: ValueType): ValueType = ValueType.Data(Id("HoleContext"), List(a, b))
  
  val composefun: BlockVar = BlockVar(Id("ctx_composeContext"), Function(Nil, Nil, List(HoleContext, HoleContext), Nil, HoleContext), Set.empty) //TODO: where do i get these in the actual compiler?
    
  val applyfun: BlockVar = BlockVar(Id("ctx_applyContext"), Function(Nil, Nil, List(HoleContext, HoleContext), Nil, HoleContext), Set.empty) //TODO: types wrong
  val emptyfun: BlockVar = BlockVar(Id("ctx_emptyContext"), Function(Nil, Nil, Nil, Nil, HoleContext), Set.empty)


  def split(context: TransformContext): (TailContext, Option[TransformContext]) = context match {
    case TransformContext.Outer(id) => (TailContext.Outer(id), None)
    case TransformContext.Val(id, Stmt.Return(Expr.Make(data, tag, targs, head :: Expr.ValueVar(id2, tpe) :: Nil)), next)
          if tag == consId && id == id2 =>
      val (init, rest) = split(next)
      (TailContext.Compose(init, TailContext.Make(data, tag, targs, List(head), Nil)), rest)
    case _ => (TailContext.Empty, Some(context))
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
          val (init, rest) = split(context)
          annotatedTpe match {
            case Function(tparams, cparams, vparams, bparams, result) => 
              val inner = Stmt.App(
                Block.BlockVar(
                  outputfun,
                  Function(tparams, cparams, vparams.appended(HoleContext), bparams, result), annotatedCapt),//TODO:capt? 
                targs,
                vargs.appended(innerReify(init)),
                bargs)
              rest match {
                case Some(rest) => reify(inner, rest, inputfun, outputfun)
                case None => inner
              }
            case _ => throw ImpossibleStateError("in an App() Statement a Function should be called")
          }
        }else{
          reify(input, context, inputfun, outputfun)
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
  
  def reify(stmt: Stmt, context: TransformContext, inputfun: Id, outputfun: Id) : Stmt = context match {
    case TransformContext.Outer(id) =>  
      val tmpId = Id("tmp")
      Stmt.Val(tmpId, stmt, Return(PureApp(applyfun, Nil, List(ValueVar(id, HoleContext), ValueVar(tmpId, stmt.tpe))))) //TODO: fix ctxType,tmpType?
    case TransformContext.Val(id, body, next) => 
      Stmt.Val(id, stmt, transform(body, inputfun, outputfun, next))
  }
  
  def innerReify(context: TailContext): Expr = context match {
    case TailContext.Empty => PureApp(emptyfun,Nil,Nil)
    case TailContext.Outer(id) => Expr.ValueVar(id,HoleContext)
    case TailContext.Make(data, tag, targs, before, after) => MakeContext(data, tag, targs, before, after)
    case TailContext.Compose(first, second) => PureApp(composefun, Nil, List(innerReify(first),innerReify(second)))
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
    val appexpected = Stmt.App(Block.BlockVar(f2, Function(Nil, Nil, List(Type.TInt, HoleContext), Nil, Type.TInt), fCapt), Nil, List(Expr.ValueVar(n, Type.TInt),Expr.ValueVar(ctx,HoleContext)), Nil)

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
    //val composefun: BlockVar = BlockVar(Id("ctx_composeContext"),Function(Nil,Nil,List(ctxType,ctxType),Nil,ctxType),Set.empty)
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
    val consctx = Expr.MakeContext(TList,consId,List(Type.TInt,Type.TInt),List(ValueVar(n,Type.TInt)),Nil)
    
    //compose(ctx,Cons(n,[]))
    val composition = Expr.PureApp(composefun,Nil,List(Expr.ValueVar(ctx,HoleContext), consctx))
    
    //f2(n-1,compose(ctx, Cons(n,[]))
    val expected = Stmt.App(Block.BlockVar(f2,Function(Nil, Nil, List(Type.TInt, HoleContext), Nil, TList),fCapt),Nil,List(minusOne, composition),Nil)
    
    val inputtransformed = transform(inputTree,f,f2,TransformContext.Outer(ctx))

    assertAlphaEquivalentStatements(inputtransformed,expected)
  }
//  val TFieldName = ValueType.Data(Id("FieldName"), Nil)
//  val fieldString1 = Expr.Literal(tailId.show,Type.TString) //proxy
//  val fieldString2 = Expr.Literal(tailId, TFieldName) //proxy

  
}