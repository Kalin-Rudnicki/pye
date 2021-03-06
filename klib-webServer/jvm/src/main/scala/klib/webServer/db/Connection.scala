package klib.webServer.db

import org.squeryl.Session

import klib.Implicits._
import klib.fp.types._

trait Connection {

  def run[T](query: Query[T]): ??[T]

}

object Connection {

  def fromSquerylSession(session: Session): Connection =
    new Connection {
      override def run[T](query: Query[T]): ??[T] =
        for {
          _ <- session.bindToCurrentThread.pure[??]
          res <- query.execute
          _ <- session.unbindFromCurrentThread.pure[??]
        } yield res
    }

}
