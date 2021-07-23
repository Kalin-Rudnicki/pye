package klib

import java.io.File

import org.eclipse.jetty.server.Server
import org.rogach.scallop._
import org.squeryl.Schema

import klib.Implicits._
import klib.fp.types._
import klib.utils._, Logger.{helpers => L}, L.Implicits._
import klib.webServer.db._

package object webServer {

  final case class ServerRes(
      dbFile: File,
      connectionFactory: ConnectionFactory,
      logger: Logger,
  )

  def makeServer(
      schema: Schema,
      routeMatcher: RouteMatcher,
      defaultPort: Int = 8080,
      defaultDbPath: String = "test-database.db",
      rbFile: Maybe[File] = None, // TODO (KR) : Make this work off of withRes, or include the rb file somehow
      withRes: ServerRes => Unit = _ => (),
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
      val handler = new ServerHandler(
        matcher = routeMatcher,
        connectionFactory = connectionFactory,
        logger = logger,
      )

      for {
        _ <- logger(
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
              _ <- logger(L.log.info(s"Database already exists at: $dbFile"))
              _ <- connectionFactory.openRunClose(schema.printDdl(s => if (!s.startsWith("--")) lb.append(s)).pure[Query])
              list = lb.toList
              _ <- logger(L.requireFlags("schema")(list.map(L.log.info)))
              _ <- rbFile match {
                case Some(rbFile) =>
                  for {
                    exists <- rbFile.exists.pure[IO]
                    _ <-
                      if (exists)
                        ("ruby" :: rbFile.toString :: dbFile.toString :: list).!.pure[IO]
                      else
                        ().pure[IO]
                  } yield ()
                case None =>
                  ().pure[IO]
              }
            } yield ()
          } else
            for {
              _ <- logger(L.log.info(s"Creating database: $dbFile"))
              _ <- connectionFactory.openRunClose(schema.create.pure[Query])
            } yield ()
        }
        server <- new Server(port).pure[IO]
        _ <- server.setHandler(handler).pure[IO]
        _ <- server.start().pure[IO]

        serverRes = ServerRes(
          dbFile = dbFile,
          connectionFactory = connectionFactory,
          logger = logger,
        )
        _ = withRes(serverRes)
      } yield ()
    }
  }

}
