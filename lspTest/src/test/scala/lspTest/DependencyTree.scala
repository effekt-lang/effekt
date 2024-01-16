package lspTest

import scala.collection.mutable.HashMap 
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.util.{Success, Failure}

class DependencyTree(implicit ec: ExecutionContext) {
  private val tree: HashMap[String, Future[String]] = HashMap("root" -> Future { "root" })

  def dependsOn(name: String, previous: String, callback: () => Future[Any]): Future[String] = {
    println(tree)
    tree.get(previous) match {
      case None => {
        // println(s"FATAL! ${name} -> ${previous}")
        throw Error("fatal dependency error")
      }
      case Some(previousFuture) => {
        // println(s"${name} dependsOn ${previous}")
        tree(name) = previousFuture.flatMap { huh =>
          // println(s"${previous} done with '${huh}', now ${name}")
          callback().transform {
            case Success(_) => Success(name)
            case Failure(error) => Failure(error)
          }
        }
        // println(tree)
        tree(name)
      }
    }
  }
}