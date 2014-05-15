package spoke

import com.stackmob.newman.ApacheHttpClient
import scala.concurrent.{ExecutionContext, Future}
import com.stackmob.newman.response.HttpResponseCode
import com.stackmob.newman.dsl._
import java.net.{URI, URL}
import com.stackmob.newman.response.HttpResponseCode.Ok
import spoke.Elements._
import spoke.report.BasicReport
import scala.concurrent.promise

object SpokeRunner extends App with Spoke {

  override implicit lazy val ec:ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  implicit val httpClient = new ApacheHttpClient

  def createRequest(url:String):Future[HttpResponseCode] =
    try {
      HEAD(encodeURL(url)).apply.map(r =>  r.code).andThen { case x => println(s"$url completed") }
    } catch {
      case x:Throwable => promise[HttpResponseCode]().failure(x).future
    }

  def getFuture(element:HtmlElement):Future[HttpResponseCode] = element match {
    case Anchor(_, link) => createRequest(link)
    case Stylesheet(link) => createRequest(link)
    case Script(link) => createRequest(link)
    case Image(_, link:String) => createRequest(link)
  }

  private def encodeURL(url:String): URL = {
    val u = new URL(url)
    //create a URI from the URL to properly encode any special characters. The extract a new URL
    new URI(u.getProtocol, u.getHost, u.getPath, u.getQuery, null).toURL
  }

  val urls = Seq("http://iys.org.au")
  BasicReport.printWithErrors(checks[HttpResponseCode](urls, getFuture, _ == Ok, CheckOptions(removeDupes = true)))
}