//

import java.io.File
import java.util.UUID

import org.eclipse.jetty.server.Server
import org.rogach.scallop._
import org.squeryl.Schema

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import pye.db._

package object pye {

  final case class ServerRes(
      dbFile: File,
      connectionFactory: ConnectionFactory,
      logger: Logger,
  )

  def standardServer(
      schema: Schema,
      resourcesRoot: File,
      api: ServerRes => RouteMatcher,
      defaultPort: Int = 8080,
      defaultDbPath: String = "test-database.db",
      routeMatcherExtra: ServerRes => RouteMatcher = _ => RouteMatcher.oneOf(),
  ): Executable = {
    import RouteMatcher._

    makeServer(
      schema = schema,
      routeMatcher = serverRes =>
        oneOf(
          method("GET")(
            complete { _ =>
              Response.Redirect("/pages").some.pure[IO]
            },
          ),
          "resources" /: method("GET")(
            any { remaining => md =>
              val remainingJoined = remaining.mkString("/")

              for {
                _ <- md.logger.log.debug(s"attempting to fetch resource: $remainingJoined")
                f <-
                  if (remaining.contains(".."))
                    IO.error(Message("Found '..' in resources path"))
                  else {
                    new File(resourcesRoot, remainingJoined).pure[IO]
                  }
              } yield Response.file(f).some
            },
          ),
          "pages" /: any { _ => _ =>
            Response.file(new File(resourcesRoot, "index.html")).some.pure[IO]
          },
          "api" /: api(serverRes),
          routeMatcherExtra(serverRes),
        ),
      defaultPort = defaultPort,
      defaultDbPath = defaultDbPath,
    )
  }

  def makeServer(
      schema: Schema,
      routeMatcher: ServerRes => RouteMatcher,
      defaultPort: Int = 8080,
      defaultDbPath: String = "test-database.db",
  ): Executable = {
    final class Conf(args: Seq[String]) extends Executable.Conf(args) {
      val port: ScallopOption[Int] = opt[Int](default = defaultPort.someOpt)
      val dbPath: ScallopOption[String] = opt[String](default = defaultDbPath.someOpt)

      verify()
    }
    object Conf extends Executable.ConfBuilder(new Conf(_))

    Executable.fromConf(Conf) { (logger, conf) =>
      val port = conf.port()
      val dbFile = new File(conf.dbPath())

      val connectionFactory = ConnectionFactory.fromSqliteFile(dbFile)
      val serverRes = ServerRes(
        dbFile = dbFile,
        connectionFactory = connectionFactory,
        logger = logger,
      )
      val handler = new ServerHandler(
        matcher = routeMatcher(serverRes),
        connectionFactory = connectionFactory,
        logger = logger,
      )

      for {
        _ <- logger.log(
          L(
            L.log.info(s"Starting server on port: $port"),
            L.log.info(s"Database path: $dbFile"),
          ),
        )
        exists <- dbFile.exists.pure[IO]
        _ <- {
          if (exists) {
            import scala.collection.mutable
            import scala.sys.process._
            val lb = mutable.ListBuffer[String]()

            for {
              _ <- logger.log.info(s"Database already exists at: $dbFile")
              _ <- connectionFactory.openRunClose(schema.printDdl(s => if (!s.startsWith("--")) lb.append(s)).pure[Query])
              list = lb.toList
              _ <- logger.log(L.requireFlags("schema")(list.map(L.log.info)))
              rbFileBytes <- getClass.getClassLoader.getResourceAsStream("ruby/db_diff.rb").readAllBytes.pure[IO]
              _ <-
                new File(s"${UUID.randomUUID}.rb")
                  .pure[IO]
                  .bracket { rbFile =>
                    for {
                      _ <- FileUtils.writeFileBytes(rbFile, rbFileBytes)
                      _ <- ("ruby" :: rbFile.toString :: dbFile.toString :: list).!.pure[IO]
                    } yield ()
                  } { rbFile => IO(rbFile.delete()) }
            } yield ()
          } else
            for {
              _ <- logger.log.info(s"Creating database: $dbFile")
              _ <- connectionFactory.openRunClose(schema.create.pure[Query])
            } yield ()
        }
        server <- new Server(port).pure[IO]
        _ <- server.setHandler(handler).pure[IO]
        _ <- server.start().pure[IO]
      } yield ()
    }
  }

}
