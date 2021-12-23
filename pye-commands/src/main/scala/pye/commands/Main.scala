package pye.commands

import klib.fp.types._
import klib.utils._

object Main {
  def main(args: Array[String]): Unit =
    Executable
      .fromSubCommands(
        "gen" -> gen.executable,
      )(args)
      .runSyncOrExit(None)
}
