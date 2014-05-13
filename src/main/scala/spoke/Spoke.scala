package spoke

import com.stackmob.newman.response.HttpResponse

object Spoke extends App with Elements with FutureWork {

  val summary = getElementSummary("http://iys.org.au")
  println("done")
}
