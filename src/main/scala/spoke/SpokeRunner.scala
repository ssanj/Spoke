package spoke

import com.stackmob.newman.ApacheHttpClient
import scala.concurrent.{ExecutionContext, Future}
import com.stackmob.newman.response.HttpResponseCode
import com.stackmob.newman.dsl._
import java.net.URL
import com.stackmob.newman.response.HttpResponseCode.Ok
import spoke.Elements._
import spoke.report.BasicReport
import scala.concurrent.promise

object SpokeRunner extends App with Spoke {

  override implicit lazy val ec:ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  implicit val httpClient = new ApacheHttpClient

  def createRequest(url:String):Future[HttpResponseCode] =
    try {
      HEAD(new URL(url)).apply.map(r =>  r.code)
    } catch {
      case x:Throwable => promise[HttpResponseCode]().failure(x).future
    }

  //TODO:Make this Option[Future[(String,HttpResponseCode)]
  def getFuture(element:HtmlElement):Future[HttpResponseCode] = element match {
    case Anchor(_, link) => createRequest(link)
    case Stylesheet(link) => createRequest(link)
    case Script(link) => createRequest(link)
    case Image(_, link:String) => createRequest(link)
    case x => sys.error("Invalid request type: %s".format(x))
  }

//  val urls = Seq("http://iys.org.au", "http://iys.org.au/events.html", "http://iys.org.au/contact.html")
  val urls = Seq("http://www.baysidetidybags.com.au/", "http://www.baysidetidybags.com.au/services/", "http://www.baysidetidybags.com.au/contact-us/")
  BasicReport.printWithErrors(checks[HttpResponseCode](urls, getFuture, _ == Ok))
}