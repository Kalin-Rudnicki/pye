package pye.commands.conf

import java.io.File

import io.circe.generic.auto._
import io.circe.parser._

import klib.Implicits._
import klib.fp.types._
import klib.utils._

final case class PyeConfig(
    projectName: String,
    basePackage: NonEmptyList[String],
    scaffolds: Map[String, ScaffoldConfig],
) {

  def srcPath(project: String, extras: String*): List[String] =
    List(
      projectName :: Nil,
      List(project, "src", "main", "scala"),
      basePackage.toList,
      project :: extras.toList,
    ).flatten

  def `package`(extras: String*): String =
    List(
      basePackage.toList,
      extras,
    ).flatten.mkString(".")

}
object PyeConfig {

  val DefaultFile: File = new File("pye-config.json")

  def fromFile(file: File): IO[PyeConfig] =
    for {
      pyeConfigFC <- FileUtils.readFile(file)
      pyeConfig <- decode[PyeConfig](pyeConfigFC).toErrorAccumulator.toIO
    } yield pyeConfig

}
