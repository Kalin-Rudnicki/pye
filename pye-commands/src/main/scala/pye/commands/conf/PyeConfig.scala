package pye.commands.conf

import klib.fp.types._

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
