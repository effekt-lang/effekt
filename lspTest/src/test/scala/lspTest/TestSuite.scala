package lspTest

import typings.node.fsMod

import scala.concurrent.{ExecutionContext, Future}
import utest.*

import scala.util.Failure

object TestSuite extends TestSuite {
  def samplesDir = "examples/benchmarks"

  // Provide a connection to the Effekt language server to the test case
  def withServer[T](testBody: ServerConnection => Future[T])
                   (implicit ec: ExecutionContext): Future[T] = {
    val serverConn = new ServerConnection
    testBody(serverConn).transformWith { testResult =>
      serverConn.cleanup(fastExit = true).transform {
        case Failure(e) => Failure(e)
        case _ => testResult
      }
    }
  }

  def withServerWithoutCleanup[T](testBody: ServerConnection => Future[T])
                   (implicit ec: ExecutionContext): Future[T] = {
    val serverConn = new ServerConnection
    testBody(serverConn)
  }

  def forEachSample(callback: String => Future[Unit])(implicit ec: ExecutionContext): Future[Unit] = {
    forEachFile(samplesDir, callback)
  }

  def forEachFile(path: String, callback: String => Future[Unit])(implicit ec: ExecutionContext): Future[Unit] = {
    // For testing a single test, uncomment:
    // callback("examples/benchmarks/are_we_fast_yet/bounce.effekt")
    linearizeFuture {
      fsMod.readdirSync(path).toSeq.flatMap { sub =>
        val subPath = s"$path/$sub"
        if (fsMod.statSync(subPath).get.isDirectory()) {
          Seq(() => forEachFile(subPath, callback)) // iterate in sub directory
        } else if (subPath.endsWith(".effekt")) {
          Seq(() => callback(subPath))
        } else {
          Seq.empty[() => Future[Unit]]
        }
      }
    }.map(_ => ())
  }

  // compared to Future.sequence, this function will also *start* the futures sequentially and not concurrently
  def linearizeFuture(in: Seq[() => Future[Unit]])(implicit ec: ExecutionContext): Future[Seq[Unit]] = {
    val builder = Seq.newBuilder[Unit]
    in.foldLeft(Future.successful(())) {
      (a, b) => a.transformWith { _ => b() }
    } map (_ => builder.result())
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

    // While the other tests use the `fastExit` mode to lower the test suite runtime,
    // this test actually waits for a clean exit with status code 0
    test("Graceful Shutdown") {
      withServerWithoutCleanup { serverConn =>
        for {
          client <- serverConn.setupServerAndConnect()
          _ <- serverConn.cleanup()
        } yield assert(serverConn.hasExited)
      }
    }

    test("Run all client tests") {
      forEachSample { file =>
        val file_stem = file.split("/").last.split("\\.")(0)
        withServer { serverConn =>
          for {
            client <- serverConn.setupServerAndConnect()
            _ <- client.initialize()
            tests = new TestSingleFile(client, file)
            results <- TestRunner.runAndPrintAsync(tests.tests, s"[$file_stem]", executor = SequentialExecutor)
          } yield {
            // use Predef to not repeat all previous error messages
            Predef.assert(results.leaves.forall(leaf => leaf.value.isSuccess), "some client tests failed")
          }
        }
      }
    }
  }
}
