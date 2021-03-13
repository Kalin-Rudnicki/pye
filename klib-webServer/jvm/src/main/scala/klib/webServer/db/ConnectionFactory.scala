package klib.webServer.db

import java.io.File

import org.squeryl.Session
import org.squeryl.adapters.SQLiteAdapter

import klib.Implicits._
import klib.fp.types._

trait ConnectionFactory {

  def produceConnection: IO[Connection]

  def openRunClose[A](query: Query[A]): IO[A] =
    for {
      connection <- produceConnection
      res <- connection.run(query)
      _ <- connection.close
    } yield res

}

object ConnectionFactory {

  def fromSqliteFile(dbFile: File): ConnectionFactory =
    new ConnectionFactory {
      override def produceConnection: IO[Connection] =
        for {
          session <-
            Session
              .create(
                java.sql.DriverManager.getConnection(s"jdbc:sqlite:$dbFile"),
                new SQLiteAdapter,
              )
              .pure[IO]
        } yield Connection.fromSquerylSession(session)
    }

}
