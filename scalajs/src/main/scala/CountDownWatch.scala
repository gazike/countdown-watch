import CountDownWatch.CurrentTime
import org.scalajs.dom
import rx.core.{Rx, Var}

import scala.concurrent.duration._
import scala.language.postfixOps

class CountDownWatch {

  private val duration: Var[Duration] = Var(0 seconds)
  val minutes = Rx(duration().toMinutes)
  val seconds = Rx((duration() - (duration().toMinutes minutes)).toSeconds)
  val currentTime = Rx(CurrentTime(minutes(), seconds(), elapsedRatio))
  private var timerHandler: Int = 0
  private var configuredDuration: Duration = 0 seconds

  def +(delta: Duration): CountDownWatch = {
    val newDuration = duration() + delta
    if (newDuration >= (0 seconds))
      duration() = newDuration
    else {
      duration() = 0 seconds
    }
    this
  }

  private def elapsedRatio: Option[Double] = {
    if (timerHandler == 0) {
      None
    } else {
      val total: Double = configuredDuration.toMillis
      val current = duration().toMillis
      Some(1.0 - Math.max(0, current) / total)
    }
  }

  def toggleTimer(): Unit = {
    if (timerHandler != 0) {
      dom.clearInterval(timerHandler)
      timerHandler = 0
      duration() = configuredDuration
    } else {
      configuredDuration = duration()
      timerHandler = dom.setInterval(() => duration() -= (1 second), 1000)
    }
  }

  def resetValue(): Unit = {

  }

  override def toString = "%02d:%02d" format(minutes(), seconds())
}

object CountDownWatch {
  def apply() = new CountDownWatch()

  case class CurrentTime(minutes: Long, seconds: Long, elapsedRatio: Option[Double]) {
    val isNegative = minutes < 0 || seconds < 0

    def absMinutes = Math.abs(minutes)

    def absSeconds = Math.abs(seconds)
  }

}


