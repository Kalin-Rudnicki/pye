package pye.commands

import klib.Implicits._
import klib.fp.types._
import klib.utils._

package object scripts {

  val executable: Executable =
    Executable.fromSubCommands(
      "css" -> CSS.executable,
      "web-server" -> WebServer.executable,
      "web-comp" -> WebComp.executable,
    )

  private[scripts] def convertHereArgs(args: List[String], projectName: String, command: String): IO[Unit] = {
    def split(args: List[String]): (Boolean, List[String]) = {
      val (hereArgs, passArgs) = args.partition(_ == "--here")
      (hereArgs.nonEmpty, passArgs)
    }

    def ifHere(here: Boolean, args: String*): List[String] =
      if (here) args.toList
      else "gnome-terminal" :: "--tab" :: "--" :: args.toList

    def passArgString(passArgs: List[String]): String =
      passArgs.map(_.unesc).mkString(" ", " ", "")

    val (isHere, passArgs) = split(args)
    val commandArgs = ifHere(isHere, "sbt", s"project $projectName", s"$command${passArgString(passArgs)}")
    IO.syscall.exitCode0(commandArgs)
  }

}
