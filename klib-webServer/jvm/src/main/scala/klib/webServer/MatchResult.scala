package klib.webServer

import java.io.{File => JavaFile}

import scalatags.Text.Frag

import klib.fp.types._

sealed trait MatchResult
object MatchResult {
  case object FailedMatch extends MatchResult
  final case class Html(html: Frag, code: Int = 200) extends MatchResult
  final case class Raw(contentType: String, content: String) extends MatchResult
  final case class File(file: JavaFile, contentType: Maybe[String] = None) extends MatchResult
}
