package klib.webServer.plugin

import sbt._
import sbt.Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object KlibWebserverPlugin extends AutoPlugin {

  override def trigger = allRequirements

  object autoImport {

    val webComp: InputKey[Unit] = inputKey("webComp")
    val webCompDir = settingKey[File]("webCompDir")

  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      webCompDir := sourceDirectory.value / "../webComp",
      webComp :=
        Def.inputTaskDyn {
          import complete.DefaultParsers._

          lazy val fast = (fastLinkJS, "fastopt")
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
                def jsFile(name: String): File = {
                  val crossTargetDir = (crossTarget in (Compile / t)).value
                  val projectName = (baseDirectory in (Compile / t)).value.getParentFile.name
                  new File(s"$crossTargetDir/$projectName-$s/$name")
                }

                val moveToDir = webCompDir.value / "js"

                val files =
                  jsFile("main.js") ::
                    (if (m) jsFile("main.js.map") :: Nil else Nil)

                moveToDir.mkdirs()
                moveToDir.listFiles.foreach { f =>
                  if (f.name.contains("main.js"))
                    f.delete()
                }
                files.foreach { f =>
                  IO.copyFile(f, new File(moveToDir, f.getName))
                }

                ()
              }
              .toTask(""),
          )
        }.evaluated,
    )

}
