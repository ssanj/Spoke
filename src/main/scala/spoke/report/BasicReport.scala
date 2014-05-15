package spoke.report

import spoke.Report.PageReport

object BasicReport {
  def print[R](results:Seq[Either[Throwable, PageReport[R]]]) {
    println()

    results.foreach { result =>
      result.fold(ex => println(s"failed with error $ex"), report => printReport(report))
    }
  }

  def printWithErrors[R](results:Seq[Either[Throwable, PageReport[R]]]) {
    print[R](results)

    results.foreach { r =>
      r.fold(ex => println(s"failed with error $ex"), report => {

        if (!report.statusOther.isEmpty || !report.errors.isEmpty) {
          val heading = s"issues for ${report.url}"
          println()
          println(heading)
          println(underline(heading.length)())
        }

        report.statusOther.foreach { e =>
          println(s"[w] ${e.element} -> ${e.result}")
        }

        report.errors.foreach { e =>
          println(s"[e] ${e.element} -> ${e.error.getMessage}")
        }
      })
    }
  }

  private def printReport[R](report:PageReport[R]) {
    println(s"${report.url} -> valid links: ${report.statusSuccess.length}, " +
      s"invalid links: ${report.statusOther.length}, errors: ${report.errors.length}")
  }


  def underlineH(heading:String)(ch:String = "-"): String = underline(heading.length)(ch)

  def underline(length:Int)(ch:String = "-"): String = Seq.fill(length)(ch).mkString
}
