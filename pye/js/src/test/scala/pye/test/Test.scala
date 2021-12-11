package pye.test

import java.time._

import monocle.macros.GenLens
import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils._
import pye._
import pye.widgets.all._

object Test {
  def main(args: Array[String]): Unit = {

    def test(_ym: => YearMonth): Unit = {
      val ym = _ym
      println(ym.lengthOfMonth)
      println(ym.lengthOfYear)
      println(ym.getMonth.name().toLowerCase.capitalize)
      println
    }

    test(YearMonth.now())
    test(YearMonth.of(2021, 12))

    println(LocalDate.of(2021, 12, 12).getDayOfWeek.getValue)

  }
}
