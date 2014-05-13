package Spoke

import com.stackmob.newman._
import com.stackmob.newman.dsl._
import scala.concurrent._
import scala.concurrent.duration._
import java.net.URL
import com.stackmob.newman.response.HttpResponse
import scala.util.{Try, Success, Failure}
import ExecutionContext.Implicits.global


object Spokeman extends App {

  implicit val httpClient = new ApacheHttpClient

  def wrapWithTry[T] = (f:Future[T]) => f.map(Success[T]).recover { case x:Throwable => Failure(x) }

  def urlToFutureResponse = (url:URL) => GET(url).apply

  def tryableFuture:URL => Future[Try[HttpResponse]] = wrapWithTry[HttpResponse] compose urlToFutureResponse

  def futureList(urlSeq:Seq[String]) {
    val urls = urlSeq map (new URL(_))

    val f = Future.traverse(urls)(url => for {
      tryWrapped <- tryableFuture(url)
    } yield (url, tryWrapped))

    val parted = f.map(_.partition(x => x._2.isSuccess))

    try {
      val result = Await.result(parted, 5 minutes)
      println("success: " + result._1.map(x => (x._1, x._2.map(r => r.code))))
      println("failure: " + result._2)
    } catch {
      case x:Throwable => println("Error! " + x.getMessage)
    }
  }

  futureList(Seq("http://iys.org.au",
                 "http://1uniqueimprints.com",
                 "http://azuritepsychotherapy.com.au",
                 "http://iys2.org.au",
                 "http://github.com",
                 "http://azuritepsychotherapy.com"))

}
