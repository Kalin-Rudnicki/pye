package klib.webServer.db

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._

// TODO (KR) : DbRead vs DbWrite (?)
// TODO (KR) : Locking (?)
final class Query[+T] private (private[db] val execute: ??[T])

object Query {

  def apply[T](t: ??[T]): Query[T] =
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
          for {
            res <- t.execute
            resF <- f.execute
          } yield resF(res)
        }

      override def pure[A](a: => A): Query[A] =
        Query {
          a.pure[??]
        }

      override def flatten[A](t: Query[Query[A]]): Query[A] =
        Query {
          for {
            outer <- t.execute
            inner <- outer.execute
          } yield inner
        }

    }

}
