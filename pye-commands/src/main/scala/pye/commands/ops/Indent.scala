package pye.commands.ops

sealed trait Indent {

  def quantStr: String =
    this match {
      case Indent.*               => "*"
      case Indent.Exactly(indent) => s"{$indent}"
      case Indent.AtLeast(indent) => s"{$indent,}"
    }

}
object Indent {
  case object * extends Indent
  final case class Exactly(indent: Int) extends Indent
  final case class AtLeast(indent: Int) extends Indent
}
