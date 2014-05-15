package spoke

import java.net.URL
import org.htmlcleaner.{TagNode, HtmlCleaner}

object Elements {

  sealed abstract class HtmlElement
  case class Anchor(name:Option[String], link:String) extends HtmlElement
  case class Stylesheet(link:String) extends HtmlElement
  case class Script(link:String) extends HtmlElement
  case class Image(name:Option[String], link:String) extends HtmlElement

  abstract sealed class ParseResult
  case class Parsed(element:HtmlElement) extends ParseResult
  case class Skipped(reason:String, tagNode:TagNode) extends ParseResult
  case class Error(reason:String, tagNode:TagNode) extends ParseResult

  case class FailureSummary(reason:String, tagNode:TagNode)
  case class ElementSummary(valid:Seq[HtmlElement], skipped:Seq[FailureSummary], failed:Seq[FailureSummary])

  val httpUrl = "^https?://.*"
  val protocoledUrl = "^.*:.*"
  val httpUnspecifiedUrl = "^//.*"

  def getElementSummary(url:String, removeDupes:Boolean): ElementSummary = {
    val elements = getElementByType(getRootNode(url), Seq("a", "link", "img", "script")).map(nodeToElement(url))
    parseHtml(if (removeDupes) elements.toSet.toSeq else elements)
  }

  private def booleanToOption[A](f:A => Boolean, value:A): Option[A] = {
    if (f(value)) Some(value) else None
  }

  private def parseHtml(parsedResults:Seq[ParseResult]): ElementSummary = {
    ElementSummary(parsedResults.collect { case Parsed(el) => el },
                   parsedResults.collect { case Skipped(r, tn) => FailureSummary(r, tn) },
                   parsedResults.collect { case Error(r, tn) => FailureSummary(r, tn) }
    )
  }

  private def getElementByType(root:TagNode, elementTypes:Seq[String]):Seq[TagNode] = {
    elementTypes.flatMap(e => root.getElementsByName(e, true).toSeq)
  }

  private def nodeToElement(url:String)(tagNode:TagNode):ParseResult = {
    import scala.collection.convert.Wrappers._

    implicit val attribs = JMapWrapper(tagNode.getAttributes).toMap

    val domain= getDomain(url)

    tagNode.getName match {
      case "a" => createHtmlElement("href", wrapUrl(domain, createAnchor(tagNode)), "Could not find href for anchor tag: [$tagName]", Error(_, tagNode))

      case "link" => createHtmlElement("href", wrapUrl(domain, s => Parsed(Stylesheet(s))), s"Could not find href for link tag: [$tagNode]", Error(_, tagNode))

      case "script" => createHtmlElement("src", wrapUrl(domain, s => Parsed(Script(s))), "Could not find src for script tag", Skipped(_, tagNode))

      case "img" => createHtmlElement("src", wrapUrl(domain, s => Parsed(Image(attribs.get("alt"), s))), s"Could not find src for Img tag: [$tagNode]", Error(_, tagNode))
    }
  }

  private def createAnchor(tagNode:TagNode)(href:String): ParseResult = {
    if (!href.matches(httpUrl)) Skipped("non-http(s): anchor link [%s -> %s]".format(tagNode.getText, href), tagNode)
    else Parsed(Anchor(booleanToOption[String](!_.isEmpty, tagNode.getText.toString.trim), href))
  }

  private def getDomain(url:String) = {
    val u = new URL(url)
    s"${u.getProtocol}://${u.getHost}${ if (u.getPort == -1) "" else u.getPort }"
  }

  private def wrapUrl(domain:String, block:(String) => ParseResult) = (link:String) => block(getAbsoluteUrl(domain, link))

  private def getAbsoluteUrl(domain:String, link:String): String = {

      if (link.matches(httpUnspecifiedUrl) || (!link.matches(httpUrl) && !link.matches(protocoledUrl))) { //relative
        if (link.matches(httpUnspecifiedUrl)) {
          //some weird urls from Squarespace start with // which simply resolves to http:// in the browser. Do the same.
          s"http:$link"
        } else if (link.startsWith("/")) domain + link else domain + "/" + link
      } else link //already absolute or not http(s).
  }

  private def createHtmlElement(key:String, block:(String) => ParseResult, reason:String, errorHandler:String => ParseResult)(implicit attributes:Map[String, String]): ParseResult = {
    attributes.get(key).map { k =>
      block(k)
    }.getOrElse(errorHandler(reason))
  }

  private def getRootNode(url:String):TagNode = {
    val cleaner = new HtmlCleaner
    cleaner.clean(new URL(url))
  }
}

