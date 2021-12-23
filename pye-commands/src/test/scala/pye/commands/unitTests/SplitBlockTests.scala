package pye.commands.unitTests

import scala.annotation.tailrec

import org.scalatest.funspec.AnyFunSpec

import klib.Implicits._
import klib.fp.types._
import pye.commands.ops._

final class SplitBlockTests extends AnyFunSpec {

  private object strs {

    val ex1: String =
      """
        |object Outer {
        |
        |  // format: off
        |  
        |  // Line #1
        |  // Line #2
        |  
        |  // format: on
        |
        |}
        |""".stripMargin

  }

  private object exp {

    def makeExp(
        inputStr: String,
        indent: Int,
        lineNo: Int,
    ): SplitBlock = {
      @tailrec
      def loop(
          remaining: Int,
          queue: List[String],
          stack: List[String],
      ): SplitBlock =
        if (remaining > 0) loop(remaining - 1, queue.tail, queue.head :: stack)
        else SplitBlock(indent, stack.reverse, queue.head, queue.tail)

      loop(
        lineNo - 1,
        stringToLines(inputStr),
        Nil,
      )
    }

    val exp1: SplitBlock = makeExp(strs.ex1, 2, 9)

  }

  describe("find") {

    def makeTest(
        caseNo: Int,
        config: SplitBlock.Config,
        inputStr: String,
        exp: Maybe[SplitBlock],
    ): Unit =
      it(s"case-$caseNo") {
        SplitBlock.find(config, stringToLines(inputStr)) match {
          case Alive(res) =>
            exp match {
              case Some(exp) =>
                assertResult(exp)(res)
              case None =>
                fail("'NestedBlock.find' succeeded, but should have failed")
            }
          case Dead(errors) =>
            exp match {
              case Some(exp) =>
                fail(
                  s"""'SplitBlock.find' failed, but should have succeeded:
                     |${errors.mkString("\n", "\n", "")}
                     |$exp""".stripMargin,
                )
              case None =>
                succeed
            }
        }
      }

    makeTest(
      1,
      SplitBlock.Config("// format: on"),
      strs.ex1,
      exp.exp1.some,
    )

    makeTest(
      2,
      SplitBlock.Config("// format: on", Indent.Exactly(2)),
      strs.ex1,
      exp.exp1.some,
    )

    makeTest(
      3,
      SplitBlock.Config("// format: on", Indent.AtLeast(2)),
      strs.ex1,
      exp.exp1.some,
    )

    makeTest(
      4,
      SplitBlock.Config("// format: on", Indent.AtLeast(3)),
      strs.ex1,
      None,
    )

  }

}
