package pye.commands.scripts

import klib.fp.types._
import klib.utils._

object CSS {

  val executable: Executable =
    Executable { (_, _) =>
      IO.syscall.exitCode0("sass", "resources/scss/styles.scss", "resources/styles.css")
    }

}
