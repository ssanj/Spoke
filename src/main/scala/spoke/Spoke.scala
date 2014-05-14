package spoke

import com.stackmob.newman._
import com.stackmob.newman.dsl._
import scala.concurrent.duration._

import com.stackmob.newman.response.HttpResponseCode
import java.net.URL
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import com.stackmob.newman.response.HttpResponseCode.Ok

object Spoke extends App with Elements with FutureWork {

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

  val summary = getElementSummary("http://iys.org.au")
  val resultsE = futureList[HtmlElement, (String, HttpResponseCode)](summary.valid, getFuture)((10 * summary.valid.length) seconds)
  resultsE.fold(x => println("Error: " + x.getMessage), results => {
    val (success, failure) = results.partition(x => x.result.isSuccess)
    val (rOk, rOther) = success.partition {
      case FutureResult(_, Success((_, Ok))) => true
      case _ => false
    }

    println("success (200): " + rOk.map(x => "%s -> %s".format(x.value, x.result.map(_._2.code))))
    println("success (other): " + rOther.map(x => "%s -> %s".format(x.value, x.result.map(_._2.code))))
    println("failure: " + failure.map(x => "%s -> %s".format(x.value, x.result)))
  })
}
