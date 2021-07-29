package klib.webServer.db

import org.squeryl.PrimitiveTypeMode

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._
import klib.fp.utils.ado
import klib.utils._

// TODO (KR) : DbRead vs DbWrite (?)
// TODO (KR) : Locking (?)
final class Query[+T] private (private[db] val execute: IO[T]) {

  def run(implicit c: Connection): IO[T] =
    c.run(this)

  def timed(label: String): Query[T] =
    for {
      startTime <- System.currentTimeMillis.pure[Query]
      _ <- println(s"Starting: $label").pure[Query]
      res <- this
      _ <- println(s"Finished: $label [${Timer.formatFlex(System.currentTimeMillis - startTime)}]").pure[Query]
    } yield res

  def transaction: Query[T] =
    Query(IO.wrapEffect { PrimitiveTypeMode.transaction(execute.execute()) })

  def inTransaction: Query[T] =
    Query(IO.wrapEffect { PrimitiveTypeMode.inTransaction(execute.execute()) })

}

object Query {

  def apply[T](t: IO[T]): Query[T] =
    new Query(t)

  implicit val queryMonad: Monad[Query] =
    new Monad[Query] {

      override def map[A, B](t: Query[A], f: A => B): Query[B] =
        Query(t.execute.map(f))

      override def apply[A, B](t: Query[A], f: Query[A => B]): Query[B] =
        Query(t.execute.apply(f.execute))

      override def pure[A](a: => A): Query[A] =
        Query(a.pure[IO])

      override def flatten[A](t: Query[Query[A]]): Query[A] =
        Query(t.execute.flatMap(_.execute))

    }

  implicit val queryTraverseList: Traverse[List, Query] =
    new Traverse[List, Query] {

      override def traverse[T](t: List[Query[T]]): Query[List[T]] =
        Query(t.map(_.execute).traverse)

    }

}
