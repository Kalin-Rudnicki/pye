package pye.commands

import klib.Implicits._
import klib.fp.types._

object Test {
  def main(args: Array[String]): Unit = {
    IO.syscall.exitCode0("sbt", s"pye-commands/run ${"A B".unesc}").runSyncOrExit(None)
  }
}
