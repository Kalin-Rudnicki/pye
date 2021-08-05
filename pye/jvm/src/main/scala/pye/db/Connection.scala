package pye.db

import org.squeryl.Session

import klib.Implicits._
import klib.fp.types._

trait Connection {

  def run[T](query: Query[T]): IO[T]

  def close: IO[Unit]

}

object Connection {

  def fromSquerylSession(session: Session): Connection =
    new Connection {

      override def run[T](query: Query[T]): IO[T] =
        for {
          _ <- session.bindToCurrentThread.pure[IO]
          res <- query.execute
          _ <- session.unbindFromCurrentThread.pure[IO]
        } yield res

      override def close: IO[Unit] =
        session.close.pure[IO]
    }

}
