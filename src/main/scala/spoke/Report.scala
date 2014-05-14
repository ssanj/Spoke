package spoke

trait Report { this:Elements =>
    
  case class LinkReport[R](element:HtmlElement, result:R)
  
  case class LinkError(element:HtmlElement, error:Throwable)
  
  case class PageReport[R](url:String, status200:Seq[LinkReport[R]], statusOther:Seq[LinkReport[R]], errors:Seq[LinkError])

}

