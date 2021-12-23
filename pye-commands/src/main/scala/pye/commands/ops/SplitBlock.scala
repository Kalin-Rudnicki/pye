package pye.commands.ops

import scala.annotation.tailrec

import klib.Implicits._
import klib.fp.types._
import klib.utils._

final case class SplitBlock(
    indent: Int,
    before: List[String],
    splitLine: String,
    after: List[String],
) { block =>

  sealed abstract class insert(f: List[String] => SplitBlock) {

    def lines(lines: List[String]): SplitBlock = {
      val strIdt = " " * indent
      f(lines.map(strIdt + _))
    }

    def string(string: String): SplitBlock =
      lines(stringToLines(string))

  }
  object insert {
    object atStart extends insert(lines => block.copy(before = lines ::: block.before))
    object beforeSplit extends insert(lines => block.copy(before = block.before ::: lines))
    object afterSplit extends insert(lines => block.copy(after = lines ::: block.after))
    object atEnd extends insert(lines => block.copy(after = block.after ::: lines))
  }

  def lines(sectionMarkers: Boolean): List[String] = {
    def section(cs: => ColorString): List[String] =
      if (sectionMarkers) cs.toString :: Nil
      else Nil

    List(
      section("[[[ SPLIT-BLOCK-START ]]]".toColorString.green),
      section("[[[ BEFORE ]]]".toColorString.blue),
      before,
      section("[[[ SPLIT-LINE ]]]".toColorString.blue),
      splitLine :: Nil,
      section("[[[ AFTER ]]]".toColorString.blue),
      after,
      section("[[[ SPLIT-BLOCK-END ]]]".toColorString.green),
    ).flatten
  }
  def lines: List[String] = lines(false)

  def toString(sectionMarkers: Boolean): String =
    lines(sectionMarkers).mkString("\n")

  override def toString: String =
    toString(true)

  def build: String = toString(false)

}
object SplitBlock {

  final case class Config(
      splitReg: String,
      indent: Indent = Indent.*,
  )

  def find(config: Config, searchStr: String): ?[SplitBlock] =
    find(config, stringToLines(searchStr))
  def find(config: Config, searchLines: List[String]): ?[SplitBlock] = {
    val reg = (s"^(([ ]${config.indent.quantStr})(${config.splitReg}))$$").r

    @tailrec
    def loop(
        queue: List[String],
        stack: List[String],
    ): ?[SplitBlock] =
      queue match {
        case head :: tail =>
          reg.findFirstMatchIn(head).toMaybe match {
            case Some(headMatch) =>
              if (headMatch.groupCount == 3)
                SplitBlock(
                  indent = headMatch.group(2).length,
                  before = stack.reverse,
                  splitLine = head,
                  after = tail,
                ).pure[?]
              else
                ?.dead(Message("Supplied config created extra capturing-groups, cant extract..."))
            case None =>
              loop(
                tail,
                head :: stack,
              )
          }
        case Nil =>
          ?.dead(Message(s"SplitBlock.find($config) : Could not find split"))
      }

    loop(
      searchLines,
      Nil,
    )
  }

}
