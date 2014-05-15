package spoke.report

import spoke.Report.{LinkReport, PageReport}
import spoke.Elements.{Image, Script, Stylesheet, Anchor}

object BasicPrinter {
  def printBasicReport[T](report:PageReport[T]) {
    val invalidHeader = "Invalid Links"
    val errorHeader = "Errors"

    println()
    println(s"${report.url} -> valid links: ${report.statusSuccess.length}, invalid links: ${report.statusOther.length}, errors: ${report.errors.length}")
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

    println()
    println(underline(40)("_"))
  }

  def underlineH(heading:String)(ch:String = "-"): String = underline(heading.length)(ch)

  def underline(length:Int)(ch:String = "-"): String = Seq.fill(length)(ch).mkString
}
