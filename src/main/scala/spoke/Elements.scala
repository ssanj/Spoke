package spoke

import java.net.URL
import org.htmlcleaner.{TagNode, HtmlCleaner}

/*
 *
 *  1. remove dupes
 *  2. Skipped and InError should be part of another Type.
 *
 */

object Elements {

  sealed abstract class HtmlElement {
    def isInError = false
    def isSkipped = false
  }

  case class Anchor(name:Option[String], link:String) extends HtmlElement

  case class Stylesheet(link:String) extends HtmlElement

  case class Script(link:String) extends HtmlElement

  case class Image(name:Option[String], link:String) extends HtmlElement

  //TODO:Skipped and InError are not really HtmlElements and should model the Success or Failure of a parse instead.
  case class Skipped(reason:String) extends HtmlElement {
    override def isSkipped = true
  }

  case class InError(reason:String) extends HtmlElement {
    override def isInError = true
  }

  case class ElementSummary(valid:Seq[HtmlElement], skipped:Seq[HtmlElement], failed:Seq[HtmlElement])

  val httpUrl = "^https?://.*"
  val protocoledUrl = "^.*:.*"

  def getElementSummary(url:String): ElementSummary = {
    val elements = getElementByType(getRootNode(url), Seq("a", "link", "img", "script")).map(nodeToElement(url))
    parseHtml(elements)
  }

  private def booleanToOption[A](f:A => Boolean, value:A): Option[A] = {
    if (f(value)) Some(value) else None
  }

  private def parseHtml(htmlElements:Seq[HtmlElement]): ElementSummary = {
    val (success, error) = htmlElements.partition(!_.isInError)
    val (valid, skipped) = success.partition(!_.isSkipped)
    ElementSummary(valid, skipped, error)
  }

  private def getElementByType(root:TagNode, elementTypes:Seq[String]):Seq[TagNode] = {
    elementTypes.flatMap(e => root.getElementsByName(e, true).toSeq)
  }

  private def nodeToElement(url:String)(tagNode:TagNode):HtmlElement = {
    import scala.collection.convert.Wrappers._

    implicit val attribs = JMapWrapper(tagNode.getAttributes).toMap

    val domain= getDomain(url)

    tagNode.getName match {
      case "a" => createHtmlElement("href", wrapUrl(domain, href => {
        if (!href.matches(httpUrl)) Skipped("non-http(s): anchor link [%s -> %s]".format(tagNode.getText, href))
        else Anchor(booleanToOption[String](!_.isEmpty, tagNode.getText.toString.trim), href)
      }), "Could not find href for anchor tag: [$tagName]")

      case "link" => createHtmlElement("href", wrapUrl(domain, Stylesheet), s"Could not find href for link tag: [$tagNode]")

      case "script" => createHtmlElement("src", wrapUrl(domain, Script), "Could not find src for script tag", Skipped)

      case "img" => createHtmlElement("src", wrapUrl(domain, Image(attribs.get("alt"), _)), s"Could not find src for Img tag: [$tagNode]")
    }
  }

  private def getDomain(url:String) = {
    val u = new URL(url)
    s"${u.getProtocol}://${u.getHost}${ if (u.getPort == -1) "" else u.getPort }"
  }

  private def wrapUrl(domain:String, block:(String) => HtmlElement) = (link:String) => block(getAbsoluteUrl(domain, link))

  private def getAbsoluteUrl(domain:String, link:String): String = {

      if (!link.matches(httpUrl) && !link.matches(protocoledUrl)) { //relative
         if (link.startsWith("/")) domain + link else domain + "/" + link
      } else link //already absolute or not http(s).
  }

  private def createHtmlElement(key:String, block:(String) => HtmlElement, reason:String, errorHandler:String => HtmlElement = InError)(implicit attributes:Map[String, String]): HtmlElement = {
    attributes.get(key).map { k =>
      block(k)
    }.getOrElse(errorHandler(reason))
  }

  private def getRootNode(url:String):TagNode = {
    val cleaner = new HtmlCleaner
    cleaner.clean(new URL(url))
  }
}

