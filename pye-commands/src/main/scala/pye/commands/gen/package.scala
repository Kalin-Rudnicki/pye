package pye.commands

import klib.utils._

package object gen {

  val executable: Executable =
    Executable.fromSubCommands(
      "scaffold" -> Scaffold.executable,
    )

}
