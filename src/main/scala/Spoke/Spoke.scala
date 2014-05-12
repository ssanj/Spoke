import java.net.URL
import org.htmlcleaner.{TagNode, HtmlCleaner}

/*
 *
 *  1. create absolute urls for all src elements by appending domain
 *  4. Thread this bad boy -> Akka or Actors
 *  5. remove dupes
 *
 */
object Spoke extends App {

  sealed abstract class HtmlElement {
    def isInError = false
    def isSkipped = false
  }
  case class Anchor(name:Option[String], link:String) extends HtmlElement

  case class Stylesheet(link:String) extends HtmlElement

  case class Script(link:String) extends HtmlElement

  case class Image(name:Option[String], link:String) extends HtmlElement

  case class Skipped(reason:String) extends HtmlElement {
    override def isSkipped = true
  }

  case class InError(reason:String) extends HtmlElement {
    override def isInError = true
  }

  def printLinks(rootNode:TagNode) {
    val elements = getElementByType(rootNode, Seq("a", "link", "img", "script")).
                    map(nodeToElement)

    val (valid, skipped, error) = parseHtml(elements)
    
    Seq("Checked", "Skipped", "In Error").zip(Seq(valid, skipped, error)).foreach { k =>
      println("--- %s ---".format(k._1))
      k._2.foreach(println)
    }
  }
  
  def parseHtml(htmlElements:Seq[HtmlElement]): (Seq[HtmlElement], Seq[HtmlElement], Seq[HtmlElement]) = {
    val (success, error) = htmlElements.partition(!_.isInError)
    val (valid, skipped) = success.partition(!_.isSkipped)
    (valid, skipped, error)
  }

  def getElementByType(root:TagNode, elementTypes:Seq[String]):Seq[TagNode] = {
    elementTypes.flatMap(e => root.getElementsByName(e, true).toSeq)
  }


  def nodeToElement(tagNode:TagNode):HtmlElement = {
    import scala.collection.convert.Wrappers._

    implicit val attribs = JMapWrapper(tagNode.getAttributes).toMap

    tagNode.getName match {
      case "a" => createHtmlElement("href", href => {
        if (href.startsWith("mailto:")) Skipped("mailto: anchor link [%s -> %s]".format(tagNode.getText, href))
        else Anchor(booleanToOption[String](!_.isEmpty, tagNode.getText.toString.trim), href)
      }, "Could not find href for anchor tag: [$tagName]")

      case "link" => createHtmlElement("href", Stylesheet, s"Could not find href for link tag: [$tagNode]")

      case "script" => createHtmlElement("src", Script, "Could not find src for script tag", Skipped)

      case "img" => createHtmlElement("src", Image(attribs.get("alt"), _), s"Could not find src for Img tag: [$tagNode]")
    }
  }

  def createHtmlElement(key:String, block:(String) => HtmlElement, reason:String, errorHandler:String => HtmlElement = InError)(implicit attributes:Map[String, String]): HtmlElement = {
    attributes.get(key).map { k =>
      block(k)
    }.getOrElse(errorHandler(reason))
  }

  def booleanToOption[A](f:A => Boolean, value:A): Option[A] = {
    if (f(value)) Some(value) else None
  }

  def getHtmlCleanFor(url:String):TagNode = {
    val cleaner = new HtmlCleaner
    cleaner.clean(new URL(url))
  }

  val url = "http://iys.org.au"
  println(s"Retrieving links for: $url")

  val startTime = System.currentTimeMillis
  printLinks(getHtmlCleanFor(url))
  println("time taken: %s ms".format(System.currentTimeMillis - startTime))

}


