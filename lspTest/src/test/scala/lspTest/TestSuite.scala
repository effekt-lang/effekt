package lspTest

import scala.concurrent.{ExecutionContext, Future}
import utest._

object TestSuite extends TestSuite {
  // Provide a connection to the Effekt language server to the test case
  def withServer[T](testBody: ServerConnection => Future[T])
                   (implicit ec: ExecutionContext): Future[T] = {
    val serverConn = new ServerConnection
    testBody(serverConn).andThen { case _ => serverConn.cleanup() }
  }

  def tests = Tests {
    implicit val ec: ExecutionContext = framework.ExecutionContext.RunNow

    test("Connect to server") {
      withServer { serverConn =>
        for {
          client <- serverConn.setupServerAndConnect()
        } yield assert(true)
      }
    }

    test("Request initialization") {
      withServer { serverConn =>
        for {
          client <- serverConn.setupServerAndConnect()
          result <- client.initialize()
        } yield {
          Checker.checkStats("initialization", result)
          assert(true)
        }
      }
    }

    test("Graceful Shutdown") {
      withServer { serverConn =>
        for {
          client <- serverConn.setupServerAndConnect()
          _ <- client.shutdown()
          _ <- client.exit()
        } yield assert(serverConn.hasExited)
      }
    }
  }
}
