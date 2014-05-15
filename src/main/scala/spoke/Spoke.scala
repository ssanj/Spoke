package spoke

import scala.concurrent.duration._

import scala.concurrent.Future

import scala.util.{Failure, Success}
import spoke.Elements.{HtmlElement, getElementSummary}
import spoke.Report.{PageReport, LinkError, LinkReport}

trait Spoke extends FutureWork {

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
    Seq[Either[Throwable, PageReport[R]]] = urls.map(u => check[R](u, f, pred, options))

  private def check[R](url:String, f:HtmlElement => Future[R], pred:R => Boolean, options:CheckOptions): Either[Throwable, PageReport[R]] = {
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
}
