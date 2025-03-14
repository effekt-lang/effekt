package lspTest

import scala.concurrent.{ExecutionContext, Future}
import utest._

// utest runs async tests in *parallel*, while we want sequential execution
// This implements a utest wrapper that chains all futures returned from tests

object SequentialExecutor extends SequentialExecutor

trait SequentialExecutor extends framework.Executor {
  implicit val ec: ExecutionContext = ExecutionContext.global

  var futureChain: Future[Any] = Future { "root" }

  override def utestWrap(path: Seq[String], runBody: => Future[Any])(implicit ec: ExecutionContext): Future[Any] = {
    futureChain = futureChain.transformWith { _ => runBody }
    futureChain
  }
}