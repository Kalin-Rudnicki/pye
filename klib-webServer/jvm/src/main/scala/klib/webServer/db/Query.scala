package klib.webServer.db

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._
import klib.fp.utils.ado
import klib.utils._

// TODO (KR) : DbRead vs DbWrite (?)
// TODO (KR) : Locking (?)
final class Query[+T] private (private[db] val execute: IO[T]) {

  def runIO(implicit c: Connection): IO[T] =
    c.run(this)

  def run(implicit c: Connection): ??[T] =
    c.run(this).wrap

  def transaction: Query[T] = {
    // TODO (KR) :
    ???
  }

  def inTransaction: Query[T] = {
    // TODO (KR) :
    ???
  }

}

object Query {

  def apply[T](t: IO[T]): Query[T] =
    new Query(t)

  implicit val queryMonad: Monad[Query] =
    new Monad[Query] {

      override def map[A, B](t: Query[A], f: A => B): Query[B] =
        Query {
          for {
            res <- t.execute
          } yield f(res)
        }

      override def apply[A, B](t: Query[A], f: Query[A => B]): Query[B] =
        Query {
          ado[IO]
            .join(
              f.execute,
              t.execute,
            )
            .map {
              case (f, t) =>
                f(t)
            }
        }

      override def pure[A](a: => A): Query[A] =
        Query {
          a.pure[IO]
        }

      override def flatten[A](t: Query[Query[A]]): Query[A] =
        Query {
          for {
            outer <- t.execute
            inner <- outer.execute
          } yield inner
        }

    }

  sealed trait QueryMT
  type QueryM[+T] = Query[Maybe[T]] @@ QueryMT

  implicit val queryMMonad: Monad[QueryM] =
    new Monad[QueryM] {
      override def map[A, B](t: QueryM[A], f: A => B): QueryM[B] =
        t.unwrap
          .map(_.map(f))
          .wrap[QueryM[B]]

      override def apply[A, B](t: QueryM[A], f: QueryM[A => B]): QueryM[B] =
        ado[Query]
          .join(
            f,
            t,
          )
          .map {
            case (uF, uT) =>
              uT.apply(uF)
          }
          .wrap[QueryM[B]]

      override def pure[A](a: => A): QueryM[A] = a.pure[Maybe].pure[Query].wrap[QueryM[A]]

      override def flatten[A](t: QueryM[QueryM[A]]): QueryM[A] =
        (
          for {
            outer <- t.unwrap
            inner <- outer.map(_.unwrap).traverse
          } yield inner.flatten
        ).wrap[QueryM[A]]
    }

}
