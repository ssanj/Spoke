package spoke

import spoke.Elements.HtmlElement

object Report {
    
  case class LinkReport[R](element:HtmlElement, result:R)
  
  case class LinkError(element:HtmlElement, error:Throwable)
  
  case class PageReport[R](url:String, statusSuccess:Seq[LinkReport[R]], statusOther:Seq[LinkReport[R]], errors:Seq[LinkError])

}

