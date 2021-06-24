package klib.webServer.test

import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._
import klib.webServer.CSS._

object Test {

  object S {

    object `block-1` extends Block {

      object `element-1` extends Element {
        object `modifier-1` extends Modifier
        object `modifier-2` extends Modifier
      }

      object `element-2` extends Element {
        object `modifier-3` extends Modifier
        object `modifier-4` extends Modifier
      }

      object `modifier-5` extends Modifier
      object `modifier-6` extends Modifier
    }

  }

  def main(args: Array[String]): Unit = {

    println
    println("> " + (S.`block-1`).classes)
    println("> " + (S.`block-1`.!).classes)

    println
    println("> " + (S.`block-1` __ S.`block-1`.`element-1`).classes)
    println("> " + (S.`block-1`.! __ S.`block-1`.`element-1`).classes)

    println
    println("> " + (S.`block-1` -- (S.`block-1`.`modifier-5`, S.`block-1`.`modifier-6`)).classes)
    println("> " + (S.`block-1`.! -- (S.`block-1`.`modifier-5`, S.`block-1`.`modifier-6`)).classes)

    div(
      S.`block-1`,
      S.`block-1` -- S.`block-1`.`modifier-6`,
      S.`block-1`.e(_.`element-1`),
      S.`block-1`.m(_.`modifier-5`, _.`modifier-6`),
    )

  }

}
