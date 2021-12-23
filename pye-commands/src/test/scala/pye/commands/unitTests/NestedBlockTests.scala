package pye.commands.unitTests

import scala.annotation.tailrec

import org.scalatest.funspec.AnyFunSpec

import klib.Implicits._
import klib.fp.types._
import klib.utils.IndentedString._
import pye.commands.ops._

class NestedBlockTests extends AnyFunSpec {

  private object strs {
    val empty: String =
      """
        |object Outer {
        |}
        |""".stripMargin
    val innerSingleLine: String =
      """
        |object Outer {
        |  object Inner {}
        |}
        |""".stripMargin
    val innerMultiLine: String =
      """
        |object Outer {
        |  object Inner {
        |    // Inner Line #1
        |    // Inner Line #2
        |  }
        |}
        |""".stripMargin
    val nestedInnerMultLine: String =
      """
        |object Outer {
        |  object Inner {
        |    object Inner {
        |      // Inner Line #1
        |      // Inner Line #2
        |    }
        |  }
        |}
        |""".stripMargin
    val doubleNestedInnerMultLine: String =
      """
        |object Outer {
        |  object Inner {
        |    object Inner {
        |      object Inner {
        |        // Inner Line #1
        |        // Inner Line #2
        |      }
        |    }
        |  }
        |}
        |""".stripMargin
    val multiInner: String =
      """
        |object Outer {
        |  object Inner1 {
        |    // Inner Line #1
        |    // Inner Line #2
        |  }
        |  object Inner2 {
        |    // Inner Line #3
        |    // Inner Line #4
        |  }
        |}
        |""".stripMargin
  }

  private object configs {
    private def stdInner(
        indent: Indent,
        innerNum: Maybe[Int] = None,
    ): NestedBlock.Config =
      NestedBlock.Config.`{}`(
        centerReg = s"object Inner${innerNum.cata(_.toString, "")}",
        startIndent = indent,
      )

    val inner_* : NestedBlock.Config = stdInner(Indent.*)
    val innerExactly2: NestedBlock.Config = stdInner(Indent.Exactly(2))
    val innerExactly4: NestedBlock.Config = stdInner(Indent.Exactly(4))
    val innerAtLeast4: NestedBlock.Config = stdInner(Indent.AtLeast(4))
    val innerAtLeast5: NestedBlock.Config = stdInner(Indent.AtLeast(5))
    val inner1_* : NestedBlock.Config = stdInner(Indent.*, 1.some)
    val inner2_* : NestedBlock.Config = stdInner(Indent.*, 2.some)
  }

  describe("find") {
    def testConfig(
        configName: String,
        config: NestedBlock.Config,
    )(
        emptyExp: Maybe[NestedBlock],
        innerSingleLineExp: Maybe[NestedBlock],
        innerMultiLineExp: Maybe[NestedBlock],
        nestedInnerMultLineExp: Maybe[NestedBlock],
        doubleNestedInnerMultLineExp: Maybe[NestedBlock],
        multiInnerExp: Maybe[NestedBlock],
    ): Unit = {
      def makeTest(
          strName: String,
          config: NestedBlock.Config,
          input: String,
          exp: Maybe[NestedBlock],
      ): Unit =
        it(strName) {
          NestedBlock.find(config, input) match {
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
                    s"""'NestedBlock.find' failed, but should have succeeded:
                       |${errors.mkString("\n", "\n", "")}
                       |$exp""".stripMargin,
                  )
                case None =>
                  succeed
              }
          }
        }

      describe(configName) {
        makeTest(
          "empty",
          config,
          strs.empty,
          emptyExp,
        )
        makeTest(
          "innerSingleLine",
          config,
          strs.innerSingleLine,
          innerSingleLineExp,
        )
        makeTest(
          "innerMultiLine",
          config,
          strs.innerMultiLine,
          innerMultiLineExp,
        )
        makeTest(
          "nestedInnerMultLine",
          config,
          strs.nestedInnerMultLine,
          nestedInnerMultLineExp,
        )
        makeTest(
          "doubleNestedInnerMultLine",
          config,
          strs.doubleNestedInnerMultLine,
          doubleNestedInnerMultLineExp,
        )
        makeTest(
          "multiInner",
          config,
          strs.multiInner,
          multiInnerExp,
        )
      }
    }

    object exp {

      private def makeExp(
          input: String,
          indent: Int,
          startLineNo: Int,
          endLineNo: Int,
      ): NestedBlock = {
        @tailrec
        def take(i: Int, queue: List[String], stack: List[String] = Nil): (List[String], List[String]) =
          if (i > 0) take(i - 1, queue.tail, queue.head :: stack)
          else (stack.reverse, queue)

        val (before, tmp1) = take(startLineNo - 1, stringToLines(input))
        val startLine = tmp1.head
        val (inBlock, tmp2) = take(endLineNo - startLineNo - 1, tmp1.tail)
        val endLine = tmp2.head
        val after = tmp2.tail

        NestedBlock(
          indent = indent,
          before = before,
          blockStart = startLine,
          inBlock = inBlock,
          blockEnd = endLine,
          after = after,
        )
      }

      val innerSingleLine: NestedBlock =
        NestedBlock(
          indent = 2,
          before = List(
            "",
            "object Outer {",
          ),
          blockStart = "  object Inner {",
          inBlock = Nil,
          blockEnd = "  }",
          after = List(
            "}",
            "",
          ),
        )
      val innerMultiLine: NestedBlock = makeExp(strs.innerMultiLine, 2, 3, 6)
      val nestedInnerMultLine2: NestedBlock = makeExp(strs.nestedInnerMultLine, 2, 3, 8)
      val nestedInnerMultLine4: NestedBlock = makeExp(strs.nestedInnerMultLine, 4, 4, 7)
      val doubleNestedInnerMultLine2: NestedBlock = makeExp(strs.doubleNestedInnerMultLine, 2, 3, 10)
      val doubleNestedInnerMultLine4: NestedBlock = makeExp(strs.doubleNestedInnerMultLine, 4, 4, 9)
      val doubleNestedInnerMultLine6: NestedBlock = makeExp(strs.doubleNestedInnerMultLine, 6, 5, 8)
      val multiInner1: NestedBlock = makeExp(strs.multiInner, 2, 3, 6)
      val multiInner2: NestedBlock = makeExp(strs.multiInner, 2, 7, 10)

    }

    // =====| Tests |=====

    testConfig(
      configName = "inner_*",
      config = configs.inner_*,
    )(
      emptyExp = None,
      innerSingleLineExp = exp.innerSingleLine.some,
      innerMultiLineExp = exp.innerMultiLine.some,
      nestedInnerMultLineExp = exp.nestedInnerMultLine2.some,
      doubleNestedInnerMultLineExp = exp.doubleNestedInnerMultLine2.some,
      multiInnerExp = None,
    )
    testConfig(
      configName = "innerExactly2",
      config = configs.innerExactly2,
    )(
      emptyExp = None,
      innerSingleLineExp = exp.innerSingleLine.some,
      innerMultiLineExp = exp.innerMultiLine.some,
      nestedInnerMultLineExp = exp.nestedInnerMultLine2.some,
      doubleNestedInnerMultLineExp = exp.doubleNestedInnerMultLine2.some,
      multiInnerExp = None,
    )
    testConfig(
      configName = "innerExactly4",
      config = configs.innerExactly4,
    )(
      emptyExp = None,
      innerSingleLineExp = None,
      innerMultiLineExp = None,
      nestedInnerMultLineExp = exp.nestedInnerMultLine4.some,
      doubleNestedInnerMultLineExp = exp.doubleNestedInnerMultLine4.some,
      multiInnerExp = None,
    )
    testConfig(
      configName = "innerAtLeast4",
      config = configs.innerAtLeast4,
    )(
      emptyExp = None,
      innerSingleLineExp = None,
      innerMultiLineExp = None,
      nestedInnerMultLineExp = exp.nestedInnerMultLine4.some,
      doubleNestedInnerMultLineExp = exp.doubleNestedInnerMultLine4.some,
      multiInnerExp = None,
    )
    testConfig(
      configName = "innerAtLeast5",
      config = configs.innerAtLeast5,
    )(
      emptyExp = None,
      innerSingleLineExp = None,
      innerMultiLineExp = None,
      nestedInnerMultLineExp = None,
      doubleNestedInnerMultLineExp = exp.doubleNestedInnerMultLine6.some,
      multiInnerExp = None,
    )
    testConfig(
      configName = "inner1_*",
      config = configs.inner1_*,
    )(
      emptyExp = None,
      innerSingleLineExp = None,
      innerMultiLineExp = None,
      nestedInnerMultLineExp = None,
      doubleNestedInnerMultLineExp = None,
      multiInnerExp = exp.multiInner1.some,
    )
    testConfig(
      configName = "inner2_*",
      config = configs.inner2_*,
    )(
      emptyExp = None,
      innerSingleLineExp = None,
      innerMultiLineExp = None,
      nestedInnerMultLineExp = None,
      doubleNestedInnerMultLineExp = None,
      multiInnerExp = exp.multiInner2.some,
    )
    testConfig(
      configName = "capturingGroupError",
      config = NestedBlock.Config.`{}`("(object Inner)"),
    )(
      emptyExp = None,
      innerSingleLineExp = None,
      innerMultiLineExp = None,
      nestedInnerMultLineExp = None,
      doubleNestedInnerMultLineExp = None,
      multiInnerExp = None,
    )
  }

  println(strs.innerSingleLine.unesc)

  describe("find+insert") {
    def makeTest(
        caseNo: Int,
        config: NestedBlock.Config,
        inputStr: String,
        insertF: (NestedBlock, String) => NestedBlock,
        insertStr: String,
        expStr: String,
    ): Unit =
      it(s"case-$caseNo") {
        NestedBlock.find(config, inputStr) match {
          case Alive(res) =>
            assertResult(expStr)(insertF(res, insertStr).build)
          case Dead(errors) =>
            fail(s"Unable to find: ${errors.mkString(", ")}")
        }
      }

    makeTest(
      1,
      configs.inner_*,
      strs.innerMultiLine,
      _.insert.afterBlockStart.string(_),
      indented(
        "<NEW-TEXT>",
        Break,
      ).toString("  "),
      """
        |object Outer {
        |  object Inner {
        |    <NEW-TEXT>
        |    
        |    // Inner Line #1
        |    // Inner Line #2
        |  }
        |}
        |""".stripMargin,
    )

    makeTest(
      2,
      configs.inner_*,
      strs.innerMultiLine,
      _.insert.afterBlockStart.string(_),
      "<NEW-TEXT>",
      """
        |object Outer {
        |  object Inner {
        |  <NEW-TEXT>
        |    // Inner Line #1
        |    // Inner Line #2
        |  }
        |}
        |""".stripMargin,
    )

    makeTest(
      3,
      configs.inner_*,
      strs.innerMultiLine,
      _.insert.afterBlockStart.string(_),
      "<NEW-TEXT>\n",
      """
        |object Outer {
        |  object Inner {
        |  <NEW-TEXT>
        |  
        |    // Inner Line #1
        |    // Inner Line #2
        |  }
        |}
        |""".stripMargin,
    )

    makeTest(
      4,
      configs.inner_*,
      strs.innerMultiLine,
      _.insert.beforeBlockEnd.string(_),
      indented(
        Break,
        "<NEW-TEXT>",
      ).toString("  "),
      """
        |object Outer {
        |  object Inner {
        |    // Inner Line #1
        |    // Inner Line #2
        |    
        |    <NEW-TEXT>
        |  }
        |}
        |""".stripMargin,
    )
  }

}
