package pye.commands.gen

import java.io.File

import org.rogach.scallop._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils._
import klib.utils._
import klib.utils.IndentedString._
import pye.commands.conf._
import pye.commands.ops._

object Scaffold {

  final class Conf(args: Seq[String]) extends Executable.Conf(args) {
    val projectDir: ScallopOption[File] = opt[File](
      descr = "Set a directory other than '.' as the project directory.",
    )

    val name: ScallopOption[String] = opt[String](
      required = true,
      descr = "Name of the scaffold.\nIs expected to be camel-case starting with an upper-case letter.",
    )

    val modelFileName: ScallopOption[String] = opt[String]()
    val modelTableName: ScallopOption[String] = opt[String]()
    val queryFile: ScallopOption[String] = opt[String]()
    val routeFileName: ScallopOption[String] = opt[String]()
    val routePathName: ScallopOption[String] = opt[String]()

    helpWidth(120)
    verify()
  }
  object Conf extends Executable.ConfBuilder(new Conf(_))

  val executable: Executable =
    Executable.fromConf(Conf) { (logger, conf) =>
      val projectDir = conf.projectDir.toOption.getOrElse(new File("."))
      object projectFile {
        def apply(strs: String*): File =
          list(strs.toList)
        def list(strs: List[String]): File =
          new File(projectDir, strs.mkString("/"))
      }

      val pyeConfigFile: File = projectFile("pye-config.json")
      def schemaFile(pyeConfig: PyeConfig): File = projectFile.list(pyeConfig.srcPath("jvm", "db", "Schema.scala"))
      def mainFile(pyeConfig: PyeConfig): File = projectFile.list(pyeConfig.srcPath("jvm", "Main.scala"))

      def readPyeConfig: IO[PyeConfig] =
        for {
          pyeConfigFC <- FileUtils.readFile(pyeConfigFile)
          pyeConfig <- decode[PyeConfig](pyeConfigFC).toErrorAccumulator.toIO
        } yield pyeConfig

      def verifyNames(pyeConfig: PyeConfig, scaffoldName: String, scaffold: ScaffoldConfig): IO[PyeConfig] = {
        val allScaffolds = pyeConfig.scaffolds.toSet
        def allBy(f: ((String, ScaffoldConfig)) => String): Set[String] =
          allScaffolds.map(f)
        def allByM(f: ((String, ScaffoldConfig)) => Maybe[String]): Set[String] =
          allScaffolds.flatMap(f(_).toOption)

        def verify(name: String, f: ((String, ScaffoldConfig)) => String): IO[Unit] = {
          val supplied = f((scaffoldName, scaffold))
          IO.errorIf(allBy(f).contains(supplied))(Message(s"Duplicate $name ($supplied)"))
        }
        def verifyM(name: String, f: ((String, ScaffoldConfig)) => Maybe[String]): IO[Unit] =
          f((scaffoldName, scaffold)) match {
            case Some(supplied) => IO.errorIf(allByM(f).contains(supplied))(Message(s"Duplicate $name ($supplied)"))
            case None           => ().pure[IO]
          }

        ado[IO]
          .join(
            verify("name", _._1),
            verify("model-file-name", _._2.model.fileName),
            verify("model-schema-table-name", _._2.model.schemaTableName),
            verify("model-db-table-name", _._2.model.dbTableName),
            verify("query-file-name", _._2.query.fileName),
            verifyM("route-file-name", _._2.route.map(_.fileName)),
            verifyM("route-path-name", _._2.route.map(_.pathName)),
          )
          .map { _ =>
            pyeConfig.copy(
              scaffolds = pyeConfig.scaffolds.updated(scaffoldName, scaffold),
            )
          }
      }

      def writeModel(pyeConfig: PyeConfig, model: ScaffoldConfig.Model): IO[Unit] = {
        val file = projectFile.list(pyeConfig.srcPath("jvm", "db", "models", s"${model.fileName}.scala"))
        val pkg = pyeConfig.`package`("jvm", "db", "models")
        val idtStr =
          inline(
            s"package $pkg",
            Break,
            "import pye.db._",
            Break,
            s"case class ${model.fileName} private[db] (",
            ") extends DbObject",
            Break,
          )

        for {
          _ <- FileUtils.writeFile(file, idtStr.toString("  "))
          schemaFC <- FileUtils.readFile(schemaFile(pyeConfig))
          splitBlock <- SplitBlock.find(SplitBlock.Config("// format: on"), schemaFC).toIO
          _ <- FileUtils.writeFile(
            schemaFile(pyeConfig),
            splitBlock.insert.beforeSplit
              .string(s"val ${model.schemaTableName}: Table[${model.fileName}] = table(${model.dbTableName.unesc})")
              .build,
          )
        } yield ()
      }

      def writeQuery(pyeConfig: PyeConfig, model: ScaffoldConfig.Model, query: ScaffoldConfig.Query): IO[Unit] = {
        val file = projectFile.list(pyeConfig.srcPath("jvm", "db", "queries", s"${query.fileName}.scala"))
        val pkg = pyeConfig.`package`("jvm", "db", "queries")
        val idtStr = {
          def section(name: String): IndentedString =
            inline(
              s"$name {",
              indented(
                "object auth {}",
                "object create {}",
                "object read {}",
                "object update {}",
                "object delete {}",
              ),
              "}",
            )

          inline(
            s"package $pkg",
            Break,
            "import org.squeryl.PrimitiveTypeMode._",
            Break,
            "import klib.Implicits._",
            "import klib.fp.types._",
            "import pye.db._",
            Break,
            s"import ${pyeConfig.`package`("jvm", "db", "{models => M, Schema => S}")}",
            s"import ${pyeConfig.`package`("shared", "{data => D}")}",
            Break,
            s"object ${query.fileName} {",
            indented(
              Break,
              s"val Q = S.${model.schemaTableName}.queries",
              Break,
              section("private object `private`"),
              Break,
              section("private[db] object db"),
              Break,
              section("object public"),
              Break,
            ),
            "}",
            Break,
          )
        }

        FileUtils.writeFile(file, idtStr.toString("  "))
      }

      def writeRoute(pyeConfig: PyeConfig, route: ScaffoldConfig.Route): IO[Unit] = {
        val file = projectFile.list(pyeConfig.srcPath("jvm", "routes", s"${route.fileName}.scala"))
        val pkg = pyeConfig.`package`("jvm", "routes")
        val idtStr =
          inline(
            s"package $pkg",
            Break,
            "import io.circe.generic.auto._",
            Break,
            "import klib.Implicits._",
            "import klib.fp.types._",
            "import pye._",
            "import pye.RouteMatcher._",
            Break,
            s"import ${pyeConfig.`package`("jvm", "db", "{models => M, queries => Q}")}",
            s"import ${pyeConfig.`package`("shared", "constants", "_")}",
            s"import ${pyeConfig.`package`("shared", "{data => D}")}",
            Break,
            s"object ${route.fileName} {",
            indented(
              Break,
              "val matcher: RouteMatcher =",
              indented(
                s""""${route.pathName}" /: oneOf(""",
                ")",
              ),
              Break,
            ),
            "}",
            Break,
          )

        for {
          _ <- FileUtils.writeFile(file, idtStr.toString("  "))
          mainFC <- FileUtils.readFile(mainFile(pyeConfig))
          nestedBlock <-
            NestedBlock
              .find(
                NestedBlock.Config.stdConfig("(", "),")("oneOf"),
                mainFC,
              )
              .toIO
          _ <- FileUtils.writeFile(
            mainFile(pyeConfig),
            nestedBlock.insert.beforeBlockEnd
              .string(s"  routes.${route.fileName}.matcher,")
              .build,
          )
        } yield ()
      }

      def writePyeConfig(pyeConfig: PyeConfig): IO[Unit] =
        FileUtils.writeFile(pyeConfigFile, pyeConfig.asJson.spaces2)

      for {
        pyeConfig <- readPyeConfig
        scaffoldName = conf.name()
        scaffold = ScaffoldConfig.defaults(
          name = scaffoldName,
          modelFileName = conf.modelFileName.toOption.toMaybe,
          modelTableName = conf.modelTableName.toOption.toMaybe,
          queryFileName = conf.queryFile.toOption.toMaybe,
          routeFileName = conf.routeFileName.toOption.toMaybe,
          routeName = conf.routePathName.toOption.toMaybe,
        )
        pyeConfig <- verifyNames(pyeConfig, scaffoldName, scaffold)
        _ <- ado[IO].join(
          writeModel(pyeConfig, scaffold.model),
          writeQuery(pyeConfig, scaffold.model, scaffold.query),
          scaffold.route.map(route => writeRoute(pyeConfig, route)).traverse,
          writePyeConfig(pyeConfig),
        )
      } yield ()
    }

}
