package pye.commands.ops

import scala.annotation.tailrec
import scala.util.matching.Regex

import klib.Implicits._
import klib.fp.types._
import klib.utils._

final case class NestedBlock(
    indent: Int,
    before: List[String],
    blockStart: String,
    inBlock: List[String],
    blockEnd: String,
    after: List[String],
) { block =>

  sealed abstract class insert(f: List[String] => NestedBlock) {

    def lines(lines: List[String]): NestedBlock = {
      val strIdt = " " * indent
      f(lines.map(strIdt + _))
    }

    def string(string: String): NestedBlock =
      lines(stringToLines(string))

  }
  object insert {
    object atStart extends insert(lines => block.copy(before = lines ::: block.before))
    object beforeBlockStart extends insert(lines => block.copy(before = block.before ::: lines))
    object afterBlockStart extends insert(lines => block.copy(inBlock = lines ::: block.inBlock))
    object beforeBlockEnd extends insert(lines => block.copy(inBlock = block.inBlock ::: lines))
    object afterBlockEnd extends insert(lines => block.copy(after = lines ::: block.after))
    object atEnd extends insert(lines => block.copy(after = block.after ::: lines))
  }

  def lines(sectionMarkers: Boolean): List[String] = {
    def section(cs: => ColorString): List[String] =
      if (sectionMarkers) cs.toString :: Nil
      else Nil

    List(
      section("[[[ NESTED-BLOCK-START ]]]".toColorString.green),
      section("[[[ BEFORE ]]]".toColorString.blue),
      before,
      section("[[[ START-LINE ]]]".toColorString.blue),
      blockStart :: Nil,
      section("[[[ IN-BLOCK ]]]".toColorString.blue),
      inBlock,
      section("[[[ END-LINE ]]]".toColorString.blue),
      blockEnd :: Nil,
      section("[[[ AFTER ]]]".toColorString.blue),
      after,
      section("[[[ NESTED-BLOCK-END ]]]".toColorString.green),
    ).flatten
  }
  def lines: List[String] = lines(false)

  def toString(sectionMarkers: Boolean): String =
    lines(sectionMarkers).mkString("\n")

  override def toString: String =
    toString(true)

  def build: String = toString(false)

}
object NestedBlock {

  final case class Config(
      centerReg: String,
      blockStartReg: String,
      blockEndReg: String,
      startIndent: Indent = Indent.*,
  )
  object Config {

    def stdConfig(
        startChar: String,
        endChar: String,
    )(
        centerReg: String,
        startIndent: Indent = Indent.*,
    ): Config =
      Config(
        centerReg = centerReg,
        blockStartReg = s"[ ]*\\$startChar",
        blockEndReg = s"\\$endChar",
        startIndent = startIndent,
      )

    def `{}`(centerReg: String, startIndent: Indent = Indent.*): Config =
      stdConfig("{", "}")(centerReg, startIndent)
    def `()`(centerReg: String, startIndent: Indent = Indent.*): Config =
      stdConfig("(", ")")(centerReg, startIndent)
    def `[]`(centerReg: String, startIndent: Indent = Indent.*): Config =
      stdConfig("[", "]")(centerReg, startIndent)

  }

  def find(config: Config, searchStr: String): ?[NestedBlock] =
    find(config, stringToLines(searchStr))
  def find(config: Config, searchLines: List[String]): ?[NestedBlock] = {
    object regs {
      private def _lineStart(indent: Indent) = s"([ ]${indent.quantStr})"
      private val _center = s"(${config.centerReg})"
      private val _blockStart = s"(${config.blockStartReg})"
      private val _blockEnd = s"(${config.blockEndReg})"

      val fullBlock: Regex =
        List(
          "^(",
          _lineStart(config.startIndent),
          _center,
          _blockStart,
          _blockEnd,
          ")$",
        ).mkString.r
      val blockStart: Regex =
        List(
          "^(",
          _lineStart(config.startIndent),
          _center,
          _blockStart,
          ")$",
        ).mkString.r
      def blockEnd(indent: Int): Regex =
        List(
          "^(",
          _lineStart(Indent.Exactly(indent)),
          _blockEnd,
          ")$",
        ).mkString.r
    }

    @tailrec
    def loop2(
        queue: List[String],
        stack: List[String],
        endReg: Regex,
    ): Maybe[(List[String], String, List[String])] =
      queue match {
        case head :: tail =>
          if (endReg.matches(head)) (stack.reverse, head, tail).some
          else loop2(tail, head :: stack, endReg)
        case Nil =>
          None
      }

    @tailrec
    def loop1(
        lineNo: Int,
        queue: List[String],
        stack: List[String],
        startLinesWithNoEnd: List[Int],
    ): ?[NestedBlock] =
      queue match {
        case head :: tail =>
          regs.fullBlock.findFirstMatchIn(head).toMaybe match {
            case Some(headMatch) =>
              if (headMatch.groupCount == 5) {
                val indent = headMatch.group(2)
                val center = headMatch.group(3)
                val start = headMatch.group(4)
                val end = headMatch.group(5)

                NestedBlock(
                  indent = indent.length,
                  before = stack.reverse,
                  blockStart = s"$indent$center$start",
                  inBlock = Nil,
                  blockEnd = s"$indent$end",
                  after = tail,
                ).pure[?]
              } else
                ?.dead(Message("Supplied config created extra capturing-groups, cant extract..."))
            case None =>
              regs.blockStart.findFirstMatchIn(head).toMaybe match {
                case Some(headMatch) =>
                  if (headMatch.groupCount == 4) {
                    val indent = headMatch.group(2).length

                    loop2(tail, Nil, regs.blockEnd(indent)) match {
                      case Some((inBlock, endLine, after)) =>
                        NestedBlock(
                          indent = indent,
                          before = stack.reverse,
                          blockStart = head,
                          inBlock = inBlock,
                          blockEnd = endLine,
                          after = after,
                        ).pure[?]
                      case None =>
                        loop1(
                          lineNo + 1,
                          tail,
                          head :: stack,
                          lineNo :: startLinesWithNoEnd,
                        )
                    }
                  } else
                    ?.dead(Message("Supplied config created extra capturing-groups, cant extract..."))
                case None =>
                  loop1(
                    lineNo + 1,
                    tail,
                    head :: stack,
                    startLinesWithNoEnd,
                  )
              }
          }
        case Nil =>
          startLinesWithNoEnd.toNel match {
            case Some(startLinesWithNoEnd) =>
              val slwne = startLinesWithNoEnd.toList.reverse.mkString("[", ", ", "]")
              ?.dead(Message(s"NestedBlock.find($config) : Found start on line(s) $slwne, but no end"))
            case None =>
              ?.dead(Message(s"NestedBlock.find($config) : Could not find start"))
          }
      }

    loop1(
      1,
      searchLines,
      Nil,
      Nil,
    )
  }

}
