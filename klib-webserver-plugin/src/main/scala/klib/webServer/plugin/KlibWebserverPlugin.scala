package klib.webServer.plugin

import sbt._, Keys._


object KlibWebserverPlugin extends AutoPlugin {

  override def trigger = allRequirements

  object autoImport {

    val klwsResourceDir = settingKey[File]("klwsResourceDir")

    lazy val webComp = InputKey[Unit]("webComp") :=
      Def.inputTaskDyn {
        import complete.DefaultParsers._
        import scala.sys.process._

        lazy val fast = (scalajs.sb., "fastopt")
        lazy val full = (fullLinkJS, "opt")

        val args: List[String] = spaceDelimited("<arg>").parsed.toList
        val (t, s) =
          if (args.contains("-F"))
            full
          else
            fast

        val m = !args.contains("-m")

        Def.sequential(
          Def
            .inputTask {
              println("Running 'webComp'...")
            }
            .toTask(""),
          Compile / t,
          Def
            .inputTask {
              def jsFile(name: String): String = {
                val crossTargetDir = (crossTarget in (Compile / t)).value
                val projectName = (baseDirectory in (Compile / t)).value.getParentFile.name
                s"$crossTargetDir/$projectName-$s/$name"
              }

              val moveToDir =
                new File(
                  (baseDirectory in (Compile / t)).value,
                  "../../resources/js",
                ).getCanonicalPath

              val files =
                jsFile("main.js") ::
                  (if (m) jsFile("main.js.map") :: Nil else Nil)

              new File(moveToDir).listFiles.foreach { f =>
                if (f.name.contains("main.js"))
                  f.delete()
              }
              files.foreach { f =>
                List("cp", f, moveToDir).!
              }

              ()
            }
            .toTask(""),
        )
      }.evaluated

  }

}
