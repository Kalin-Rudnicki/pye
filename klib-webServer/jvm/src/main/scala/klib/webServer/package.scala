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

  def makeServer(
      schema: Schema,
      routeMatcher: RouteMatcher,
      defaultPort: Int = 8080,
      defaultDbPath: String = "test-database.db",
      rbFile: Maybe[File] = None,
  ): Executable =
    new Executable.ExecFromConf {

      final class Conf(args: Seq[String]) extends ScallopConf(args) {
        val port: ScallopOption[Int] = opt[Int](default = defaultPort.someOpt)
        val dbPath: ScallopOption[String] = opt[String](default = defaultDbPath.someOpt)

        verify()
      }

      override def buildConf(args: Seq[String]): Conf =
        new Conf(args)

      override def run(logger: Logger, conf: Conf): ??[Unit] = {
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
          ).wrap
          exists <- dbFile.exists.pure[??]
          _ <- {
            if (exists) {
              import scala.collection.mutable
              import scala.sys.process._
              val lb = mutable.ListBuffer[String]()

              for {
                _ <- logger(L.log.info(s"Database already exists at: $dbFile"))
                _ <- connectionFactory.openRunClose(schema.printDdl(s => if (!s.startsWith("--")) lb.append(s)).pure[Query])
                list = lb.toList
                _ <- logger(list.map(L.log.info))
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
              connectionFactory.openRunClose(schema.create.pure[Query])
          }.wrap
          server <- new Server(port).pure[??]
          _ <- server.setHandler(handler).pure[??]
          _ <- server.start().pure[??]
        } yield ()
      }

    }

}
