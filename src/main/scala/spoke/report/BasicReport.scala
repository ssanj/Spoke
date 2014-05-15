package spoke.report

import spoke.Report.PageReport
import spoke.Elements.Image

object BasicReport {
  def print[R](results:Seq[Either[Throwable, PageReport[R]]]) {

    def printReport(report:PageReport[R]) {
      println(s"${report.url} -> valid links: ${report.statusSuccess.length}, " +
        s"invalid links: ${report.statusOther.length}, errors: ${report.errors.length}")
    }

    println()

    results.foreach { result =>
      result.fold(ex => println(s"failed with error $ex"), report => printReport(report))
    }
  }

  def underlineH(heading:String)(ch:String = "-"): String = underline(heading.length)(ch)

  def underline(length:Int)(ch:String = "-"): String = Seq.fill(length)(ch).mkString
}
