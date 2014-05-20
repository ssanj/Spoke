package spoke

import scala.concurrent.duration._

import scala.concurrent.Future

import scala.util.{Failure, Success}
import spoke.Elements.{Anchor, HtmlElement, getElementSummary}
import spoke.Report.{PageReport, LinkError, LinkReport}
import java.net.URL

trait Spoke extends FutureWork {

  type FailureOrReport[R] = Either[Throwable, PageReport[R]]

  val subUrlExtensions = Seq(".html", ".htm", "/")

  private def collectLinkReports[R](results:Seq[FutureResult[HtmlElement, R]]):Seq[LinkReport[R]] = {
    results.collect { case FutureResult(el, Success(value)) => LinkReport(el, value) }
  }

  private def collectLinkErrors[R](results:Seq[FutureResult[HtmlElement, R]]):Seq[LinkError] = {
    results.collect { case FutureResult(el, Failure(ex)) => LinkError(el, ex) }
  }

  case class CheckOptions(secondsPerUrl:Int = 10, removeDupes:Boolean = false)

  /**
   * Checks the links on the provided url
   * @param urls The pages to check the links on.
   * @param f A function that maps an HtmlElement to a Future[R]
   * @param pred A function that takes an R and returns true if it matches your criteria of success or false if not.
   *             The values that return true will be returned in PageReport.statusSuccess and the values that return
   *             false will be returned in PageReport.statusOther.
   * @param options Options to prime the checker with. A very Javascripty solution.
   * @tparam R The return type from your Future.
   * @return A PageReport object with link check information.
   *         @see PageReport
   */
  def checks[R](urls:Seq[String], f:HtmlElement => Future[R], pred:R => Boolean, options:CheckOptions = CheckOptions()):
    Seq[FailureOrReport[R]] = urls.map(u => check[R](u, f, pred, options))

  private def check[R](url:String, f:HtmlElement => Future[R], pred:R => Boolean, options:CheckOptions): FailureOrReport[R] = {
    println(s"processing [$url]")
    val summary = getElementSummary(url, options.removeDupes)

    val resultsE = futureList[HtmlElement, R](summary.valid, f)(Duration(options.secondsPerUrl * summary.valid.length, SECONDS))
    resultsE.fold(x => Left(x), results => {
      val (success, failures) = results.partition(x => x.result.isSuccess)
      val (rOk, rOther) = success.partition {
        case FutureResult(_, Success(value)) => pred(value)
      }

      val statusSuccess = collectLinkReports(rOk)
      val statusOther = collectLinkReports(rOther)
      val errors = collectLinkErrors(failures)

      println(s"processed [$url]")
      Right(PageReport[R](url, statusSuccess, statusOther, errors))
    })
  }

  def checkSite[R](url:String, f:HtmlElement => Future[R], pred:R => Boolean, options:CheckOptions): Seq[FailureOrReport[R]] = {

    def doCheck(checked:Set[String], toCheck:Set[String]):Set[FailureOrReport[R]] = {
      if (toCheck.isEmpty) {
        Set.empty[FailureOrReport[R]]
      } else {
        val results:Set[FailureOrReport[R]] = toCheck.map(check[R](_, f, pred, options))
        results.flatMap(r => filterSublinks(checked, toCheck, r))
      }
    }

    def filterSublinks(checked:Set[String], toCheck:Set[String], fr:FailureOrReport[R]):Set[FailureOrReport[R]] = {
       fr.fold(x => Set(Left(x)), report => {
         val checked1 = checked + report.url
         val toCheck1 = report.statusSuccess.collect {
           case LinkReport(Anchor(_, link), _) if validSubUrl(report.url, link) => link
         }.toSet.filterNot(checked1.contains)
         doCheck(checked1, toCheck1) + Right(report)
       })
    }

    def validSubUrl(rUrl:String, link:String): Boolean = {
      try {
        val reportUrl = new URL(rUrl)
        val linkUrl = new URL(link)
        val noExtensionRegx = "^.+\\.[^.]*$"

        reportUrl.getHost == linkUrl.getHost &&
          (subUrlExtensions.exists(u => linkUrl.getPath.endsWith(u)) || !linkUrl.getPath.matches(noExtensionRegx))
      } catch {
        case x:Exception => println(s"skipping invalid url:%s for domain: %s", link, rUrl); false
      }
    }

    doCheck(Set.empty[String], Set(url)).toSeq
  }
}