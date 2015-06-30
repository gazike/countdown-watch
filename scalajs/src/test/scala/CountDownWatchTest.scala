import CountDownWatch._
import utest._
import utest.framework.TestSuite

import scala.concurrent.duration._
import scala.language.postfixOps

object CountDownWatchTest extends TestSuite {

  val tests = TestSuite {
    val uut = CountDownWatch()
    'newCountDownWatch {
      'secondsShouldReturnZero {
        assert(uut.seconds() == 0)
      }
      'minutesShouldReturnZero {
        assert(uut.minutes() == 0)
      }
    }
    'CountDownWatchPlus {
      '_1Second {
        uut + (1 second)
        assert(uut.minutes() == 0)
        assert(uut.seconds() == 1)
      }
      '_1Minute {
        uut + (1 minute)
        assert(uut.minutes() == 1)
        assert(uut.seconds() == 0)
      }
      '_1Minute1Second {
        (uut + (1 minute)) + (1 second)
        assert(uut.minutes() == 1)
        assert(uut.seconds() == 1)
      }
      '_120Second {
        uut + (120 seconds)
        assert(uut.minutes() == 2)
        assert(uut.seconds() == 0)
      }
      '_120SecondsMinus1Second {
        (uut + (120 seconds)) + (-1 second)
        assert(uut.minutes() == 1)
        assert(uut.seconds() == 59)
      }
      '_1SecondMinus120Second {
        (uut + (1 second)) + (-120 seconds)
        assert(uut.minutes() == 0)
        assert(uut.seconds() == 0)
      }
    }
  }
}
