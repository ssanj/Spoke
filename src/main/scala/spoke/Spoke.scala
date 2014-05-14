package spoke

import com.stackmob.newman._
import com.stackmob.newman.dsl._
import scala.concurrent.duration._

import com.stackmob.newman.response.HttpResponseCode
import java.net.URL
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import com.stackmob.newman.response.HttpResponseCode.Ok

object Spoke extends App with Elements with FutureWork with Report {

  implicit val httpClient = new ApacheHttpClient

  //if url is invalid will blow up the full stack.
  def createRequest(url:String):Future[HttpResponseCode] =
    HEAD(new URL(url)).apply.
        map(r =>  r.code).
        andThen { case Success(_) =>  println(s"[$url] completed") }

  //Make this Option[Future[(String,HttpResponseCode)]
  def getFuture(element:HtmlElement):Future[HttpResponseCode] = element match {
    case Anchor(_, link) => createRequest(link)
    case Stylesheet(link) => createRequest(link)
    case Script(link) => createRequest(link)
    case Image(_, link:String) => createRequest(link)
    case x => sys.error("Invalid request type: %s".format(x))
  }

  def collectLinkReports[R](results:Seq[FutureResult[HtmlElement, R]]):Seq[LinkReport[R]] = {
    results.collect { case FutureResult(el, Success(value)) => LinkReport(el, value) }
  }

  def collectLinkErrors[R](results:Seq[FutureResult[HtmlElement, R]]):Seq[LinkError] = {
    results.collect { case FutureResult(el, Failure(ex)) => LinkError(el, ex) }
  }

  def check[R](url:String, f:HtmlElement => Future[R], secondsPerUrl:Int = 10): Either[Throwable, PageReport[R]] = {
    val summary = getElementSummary(url)

    val resultsE = futureList[HtmlElement, R](summary.valid, f)((secondsPerUrl * summary.valid.length) seconds)
    resultsE.fold(x => Left(x), results => {
      val (success, failures) = results.partition(x => x.result.isSuccess)
      val (rOk, rOther) = success.partition {
        case FutureResult(_, Success((_, Ok))) => true
        case _ => false
      }

      val status200 = collectLinkReports(rOk)
      val statusOther = collectLinkReports(rOther)
      val errors = collectLinkErrors(failures)

      Right(PageReport[R](url, status200, statusOther, errors))
    })
  }

   val result = check[HttpResponseCode]("http://iys.org.au", getFuture)
   result.fold(ex => println(s"failed with error $ex"), report => println(report))
}
