package spoke

import com.stackmob.newman._
import com.stackmob.newman.dsl._
import scala.concurrent._
import scala.concurrent.duration._
import java.net.URL
import com.stackmob.newman.response.HttpResponse
import scala.util.{Try, Success, Failure}
import ExecutionContext.Implicits.global

trait FutureWork {

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

//object FutureWorkRunner extends FutureWork with App {
//
//  val resultsE = futureList[String, HttpResponse](Seq("http://iys.org.au",
//                                 "http://1uniqueimprints.com",
//                                 "http://azuritepsychotherapy.com.au",
//                                 "http://iys2.org.au",
//                                 "http://github.com",
//                                 "http://azuritepsychotherapy.com"), u => GET(new URL(u)).apply)(5 minutes)
//
//  resultsE.fold(x => println("Error: " + x.getMessage), results => {
//    val (success, failure) = results.partition(x => x.result.isSuccess)
//    println("success: " + success.map(x => "%s -> %s".format(x.value, x.result.map(_.code))))
//    println("failure: " + failure.map(x => "%s -> %s".format(x.value, x.result)))
//  })
//}
