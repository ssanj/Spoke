package spoke

import com.stackmob.newman.ApacheHttpClient
import scala.concurrent.{ExecutionContext, Future}
import com.stackmob.newman.response.HttpResponseCode
import com.stackmob.newman.dsl._
import java.net.URL
import scala.util.Success
import com.stackmob.newman.response.HttpResponseCode.Ok

object SpokeRunner extends App with Spoke {

  override implicit lazy val ec:ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  implicit val httpClient = new ApacheHttpClient

  //TODO:if url is invalid will blow up the full stack.
  def createRequest(url:String):Future[HttpResponseCode] =
    HEAD(new URL(url)).apply.
      map(r =>  r.code).
      andThen { case Success(_) =>  println(s"[$url] completed") }

  //TODO:Make this Option[Future[(String,HttpResponseCode)]
  def getFuture(element:HtmlElement):Future[HttpResponseCode] = element match {
    case Anchor(_, link) => createRequest(link)
    case Stylesheet(link) => createRequest(link)
    case Script(link) => createRequest(link)
    case Image(_, link:String) => createRequest(link)
    case x => sys.error("Invalid request type: %s".format(x))
  }

  object Printer {
    def printBasicReport[T](report:PageReport[T]) {
      val invalidHeader = "Invalid Link Detail"
      val errorHeader = "Error Detail"

      println(underline(20)("_"))
      println()
      println(s"valid links: ${report.statusSuccess.length}")
      println(s"invalid links: ${report.statusOther.length}")
      println(s"errors: ${report.errors.length}")
      println()

      if (!report.statusOther.isEmpty) {
        println(invalidHeader)
        println(underlineH(invalidHeader)())
        report.statusOther.foreach {
          case LinkReport(Anchor(name, link), r) => println(s"[anchor text='${name.getOrElse("__none__")}' href='$link'] -> $r")
          case LinkReport(Stylesheet(link), r) => println(s"[link href='$link'] -> $r")
          case LinkReport(Script(link), r) => println(s"[script src='$link'] -> $r")
          case LinkReport(Image(name, link), r) => println(s"[img text='${name.getOrElse("__none__")}' src='$link'] -> $r")
        }
      }

      if (!report.errors.isEmpty) {
        println()
        println(errorHeader)
        println(underlineH(errorHeader)())
        report.errors.foreach { e =>
          println(s"${e.element} -> ${e.error.getMessage}")
        }
      }

      println(underline(40)("="))
    }

    def underlineH(heading:String)(ch:String = "-"): String = underline(heading.length)(ch)

    def underline(length:Int)(ch:String = "-"): String = Seq.fill(length)(ch).mkString
  }

  val urls = Seq("http://iys.org.au", "http://iys.org.au/events.html", "http://iys.org.au/contact.html")
  import Printer._
  
  private def checkUrl(url:String):Either[Throwable, PageReport[HttpResponseCode]] = {
    val heading = s"Spoking $url"
    println()
    println(heading)
    println(underlineH(heading)("_"))
    check[HttpResponseCode](url, getFuture, _ == Ok)
  }
  
  private def printResult(result:Either[Throwable, PageReport[HttpResponseCode]]) {
    result.fold(ex => println(s"failed with error $ex"), report => printBasicReport(report))
  }

  urls.foreach { u => printResult(checkUrl(u)) }
}
