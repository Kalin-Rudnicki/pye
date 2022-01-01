package pye.commands.scripts

import klib.Implicits._
import klib.utils._
import pye.commands.conf._

object WebServer {

  val executable: Executable =
    Executable { (_, args) =>
      for {
        pyeConfig <- PyeConfig.fromFile(PyeConfig.DefaultFile)
        _ <- convertHereArgs(args, s"${pyeConfig.projectName}JVM", "run")
      } yield ()
    }

}
