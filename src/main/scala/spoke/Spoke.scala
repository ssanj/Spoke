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

  val requestUrlHeader = "spoke.request.url"

  //if url is invalid will blow up the full stack.
  def createRequest(url:String):Future[(String, HttpResponseCode)] =
    HEAD(new URL(url)).
      addHeaders(requestUrlHeader -> url).apply.
        map(r =>  (url, r.code)).
        andThen { case Success((u, _)) =>  println(s"$u completed") }

  //Make this Option[Future[(String,HttpResponseCode)]
  def getFuture(element:HtmlElement):Future[(String, HttpResponseCode)] = element match {
    case Anchor(_, link) => createRequest(link)
    case Stylesheet(link) => createRequest(link)
    case Script(link) => createRequest(link)
    case Image(_, link:String) => createRequest(link)
    case x => sys.error("Invalid request type: %s".format(x))
  }

  def toLinkReport[R](results:Seq[FutureResult[HtmlElement, R]]):Seq[LinkReport[R]] = {
    results.foldLeft(Seq.empty[LinkReport[R]]) {
      case (acc, FutureResult(el, Success(value))) => LinkReport(el, value) +: acc
      case (acc, _) => acc
    }
  }

  def toLinkError[R](results:Seq[FutureResult[HtmlElement, R]]):Seq[LinkError] = {
    results.foldLeft(Seq.empty[LinkError]) {
      case (acc, FutureResult(el, Failure(ex))) => LinkError(el, ex) +: acc
      case (acc, _) => acc
    }
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

      val status200 = toLinkReport(rOk)
      val statusOther = toLinkReport(rOther)
      val errors = toLinkError(failures)

      Right(PageReport[R](url, status200, statusOther, errors))
    })
  }

   val result = check[(String, HttpResponseCode)]("http://iys.org.au", getFuture)
   result.fold(ex => println(s"failed with error $ex"), report => println(report))
}
