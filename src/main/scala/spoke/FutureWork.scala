package spoke

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.Try
import scala.util.Failure
import scala.util.Success

trait FutureWork {

  implicit lazy val ec:ExecutionContext = sys.error("Please mixin an implicit instance of ExecutionContext by overriding ec in a subclass.")

  case class FutureResult[T,U](value:T, result:Try[U])

  def wrapWithTry[T] = (f:Future[T]) => f.map(Success[T]).recover { case x:Throwable => Failure(x) }

  def futureList[T,U](elements:Seq[T], t: T => Future[U])(duration: => Duration):Either[Throwable, Seq[FutureResult[T, U]]] = {

    val f = Future.traverse(elements)(element => for {
      tryWrapped <- wrapWithTry(t(element))
    } yield FutureResult(element, tryWrapped))

    try {
      Right(Await.result(f, duration))
    } catch {
      case x:Throwable => Left(x)
    }
  }
}