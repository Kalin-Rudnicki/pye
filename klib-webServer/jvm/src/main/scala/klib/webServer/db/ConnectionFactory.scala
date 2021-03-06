package klib.webServer.db

import java.io.File

import org.squeryl.Session
import org.squeryl.adapters.SQLiteAdapter

import klib.Implicits._
import klib.fp.types._

trait ConnectionFactory {

  def produceConnection: ??[Connection]

}

object ConnectionFactory {

  def fromSqliteFile(dbFile: File): ConnectionFactory =
    new ConnectionFactory {
      override def produceConnection: ??[Connection] =
        for {
          session <-
            Session
              .create(
                java.sql.DriverManager.getConnection(s"jdbc:sqlite:$dbFile"),
                new SQLiteAdapter,
              )
              .pure[??]
        } yield Connection.fromSquerylSession(session)
    }

}
