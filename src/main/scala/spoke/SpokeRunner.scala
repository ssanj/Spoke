package spoke

import com.stackmob.newman.ApacheHttpClient
import scala.concurrent.{ExecutionContext, Future}
import com.stackmob.newman.response.HttpResponseCode
import com.stackmob.newman.dsl._
import java.net.URL
import com.stackmob.newman.response.HttpResponseCode.Ok
import spoke.Elements._
import spoke.Report._

object SpokeRunner extends App with Spoke {

  override implicit lazy val ec:ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  implicit val httpClient = new ApacheHttpClient

  //TODO:if url is invalid will blow up the full stack.
  def createRequest(url:String):Future[HttpResponseCode] =
    HEAD(new URL(url)).apply.
      map(r =>  r.code)

  //TODO:Make this Option[Future[(String,HttpResponseCode)]
  def getFuture(element:HtmlElement):Future[HttpResponseCode] = element match {
    case Anchor(_, link) => createRequest(link)
    case Stylesheet(link) => createRequest(link)
    case Script(link) => createRequest(link)
    case Image(_, link:String) => createRequest(link)
    case x => sys.error("Invalid request type: %s".format(x))
  }

  val urls = Seq("http://iys.org.au", "http://iys.org.au/events.html", "http://iys.org.au/contact.html")
  import report.BasicPrinter._

  private def printResult(result:Either[Throwable, PageReport[HttpResponseCode]]) {
    result.fold(ex => println(s"failed with error $ex"), report => printBasicReport(report))
  }

  val results = checks[HttpResponseCode](urls, getFuture, _ == Ok)
  results.foreach { printResult }
}