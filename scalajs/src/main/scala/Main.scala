import org.scalajs.dom
import org.scalajs.jquery.{JQuery, JQueryEventObject, jQuery}
import rx.core.Obs

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.scalajs.js
import scala.scalajs.js.Any._
import scala.scalajs.js.Dynamic.global


object Main extends js.JSApp {

  case class Color(r: Int, g: Int, b: Int)

  val CssVisibilityProp = "visibility"
  val CssVisible: String = "visible"
  val CssHidden: String = "hidden"
  val MinusSelector: String = ".minus"
  val ValButtonsSelector: String = ".val-buttons"
  val ValueChangeShortInterval: Double = 50
  val ValueChangeLongInterval: Double = 500
  val ColorConfig = Color(r = 0x59, g = 0x92, b = 0x90)
  val ColorStart = Color(r = 0x33, g = 0x99, b = 0x33)
  val ColorZero = Color(r = 0xBB, g = 0x44, b = 0x00)

  val countDownWatch: CountDownWatch = CountDownWatch()
  var controlsVisible = true

  def main(): Unit = {
    jQuery(dom.document).ready(() => process())
  }

  val watchObs = Obs(countDownWatch.currentTime) {
    val curTime = countDownWatch.currentTime()
    if (curTime.isNegative) show(MinusSelector) else hide(MinusSelector)
    jQuery("#minutes-value").text("%02d".format(curTime.absMinutes))
    jQuery("#seconds-value").text("%02d".format(curTime.absSeconds))
    println(curTime.elapsedRatio)
    curTime.elapsedRatio.foreach(elapsedRatio => setBackgroundColor(mkColorString(getColor(elapsedRatio))))
  }


  var timerInterval: Int = 0

  def process(): Unit = {
    bindValueChangeTo("#increase-minutes", () => changeValue(1 minute)(countDownWatch))
    bindValueChangeTo("#decrease-minutes", () => changeValue(-1 minute)(countDownWatch))
    bindValueChangeTo("#increase-seconds", () => changeValue(1 second)(countDownWatch))
    bindValueChangeTo("#decrease-seconds", () => changeValue(-1 second)(countDownWatch))
    val startFunction = (e: JQueryEventObject) => toggleTimer(countDownWatch)
    jQuery("body").keypress(startFunction)
    setBackgroundColor(mkColorString(ColorConfig))
    jQuery(".value").mouseup(startFunction)
    countDownWatch + (2 minutes)
  }

  def getColor(elapsedRatio: Double): Color = {
    def interpolate(start: Int, end: Int, k: Double): Int = {
      Math.round(start * (1 - k) + end * k).toInt
    }
    val yellowRatio: Double = 0.8
    val redK = if (elapsedRatio > yellowRatio) 1.0 else elapsedRatio / yellowRatio
    val greenK = if (elapsedRatio < yellowRatio) 0 else (elapsedRatio - yellowRatio) / (1 - yellowRatio)
    println(s"redK=$redK")
    println(s"greenK=$greenK")
    Color(
      r = interpolate(ColorStart.r, ColorZero.r, redK),
      g = interpolate(ColorStart.g, ColorZero.g, greenK),
      b = interpolate(ColorStart.b, ColorZero.b, redK)
    )
  }

  def mkColorString(color: Color): String = {
    "#%02x%02x%02x".format(color.r, color.g, color.b)
  }

  def setBackgroundColor(color: String): Unit = {
    println(color)
    jQuery("body").css("background-color", color)
  }

  def hide(selector: String): Unit = {
    jQuery(selector).css(CssVisibilityProp, CssHidden)
  }

  def show(selector: String): Unit = {
    jQuery(selector).css(CssVisibilityProp, CssVisible)
  }

  def toggleTimer(countDownWatch: CountDownWatch): js.Any = {
    countDownWatch.toggleTimer()
    if (controlsVisible) hide(ValButtonsSelector) else show(ValButtonsSelector)
    controlsVisible = !controlsVisible
    if (controlsVisible) setBackgroundColor(mkColorString(ColorConfig)) else setBackgroundColor(mkColorString(ColorStart))
  }

  def bindValueChangeTo(selector: String, f: () => js.Any): js.Any = {
    val button: JQuery = jQuery(selector)
    button.mousedown((e: JQueryEventObject) => startValueChange(f))
    button.mouseup((e: JQueryEventObject) => stopValueChange())
  }

  def stopValueChange(): js.Any = {
    dom.clearInterval(timerInterval)
  }


  def startValueChange(f: () => js.Any): js.Any = {
    dom.clearInterval(timerInterval)
    f()
    def scheduleOtherChanges: () => js.Any = () => {
      f()
      dom.clearInterval(timerInterval)
      timerInterval = dom.setInterval(f, ValueChangeShortInterval)
    }
    timerInterval = dom.setInterval(scheduleOtherChanges, ValueChangeLongInterval)
  }

  def printMessage(message: Any): Unit = {
    global.console.log(String.valueOf(message))
  }

  def changeValue(increment: Duration)(watch: CountDownWatch): Unit = {
    watch + increment
  }
}
