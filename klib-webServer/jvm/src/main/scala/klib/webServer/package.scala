package klib

import java.io.File

import org.eclipse.jetty.server.Server
import org.rogach.scallop._

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import klib.webServer.db._

package object webServer {

  def makeServer(
      routeMatcher: RouteMatcher,
      defaultPort: Int = 8080,
      defaultDbPath: String = "test-database.db",
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
          _ <- logger() { src =>
            src.info(s"Starting server on port: $port")
            src.info(s"Database path: $dbFile")
          }.wrap
          server <- new Server(port).pure[??]
          _ <- server.setHandler(handler).pure[??]
          _ <- server.start.pure[??]
        } yield ()
      }

    }

}
