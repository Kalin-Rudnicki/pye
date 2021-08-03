package klib.webServer

import scala.concurrent.{ExecutionContext, Future, Promise}
import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._

final class WrappedFuture[+T] private (val future: Future[?[T]]) {

  def onComplete(f: T => Unit)(implicit ec: ExecutionContext, errorHandler: ErrorHandler): Unit =
    future.onComplete {
      _.to_?.flatten match {
        case Alive(r) =>
          f(r)
        case Dead(errors) =>
          errors.foreach(errorHandler)
      }
    }

}
object WrappedFuture {

  def apply[T](future: Future[?[T]]): WrappedFuture[T] =
    new WrappedFuture[T](future)

  def wrapFuture[T](future: Future[T])(implicit ec: ExecutionContext): WrappedFuture[T] =
    WrappedFuture(future.map(_.pure[?]))

  def wrap_?[T](ea: => ?[T])(implicit ec: ExecutionContext): WrappedFuture[T] =
    WrappedFuture(Future(ea))

  def wrapValue[T](t: => T)(implicit ec: ExecutionContext): WrappedFuture[T] =
    WrappedFuture(Future(Alive(t)))

  implicit def wrappedFutureMonad(implicit ec: ExecutionContext): Monad[WrappedFuture] =
    new Monad[WrappedFuture] {

      override def map[A, B](t: WrappedFuture[A], f: A => B): WrappedFuture[B] = {
        val p: Promise[?[B]] = Promise()
        t.future.onComplete { t =>
          p.success(t.to_?.flatten.map(f))
        }

        new WrappedFuture[B](p.future)
      }

      override def apply[A, B](t: WrappedFuture[A], f: WrappedFuture[A => B]): WrappedFuture[B] = {
        val p: Promise[?[B]] = Promise()

        f.future.onComplete { fT =>
          t.future.onComplete { tT =>
            val f_? = fT.to_?.flatten
            val t_? = tT.to_?.flatten

            p.success(t_?.apply(f_?))
          }
        }

        new WrappedFuture[B](p.future)
      }

      override def pure[A](a: => A): WrappedFuture[A] =
        WrappedFuture.wrapValue(a)

      override def flatMap[A, B](t: WrappedFuture[A], f: A => WrappedFuture[B]): WrappedFuture[B] =
        new WrappedFuture[B](
          t.future.flatMap {
            case Alive(r) =>
              f(r).future
            case dead @ Dead(_) =>
              Future(dead)
          },
        )

    }

  implicit def wrappedFutureTraverseList(implicit ec: ExecutionContext): Traverse[List, WrappedFuture] =
    new Traverse[List, WrappedFuture] {

      override def traverse[T](t: List[WrappedFuture[T]]): WrappedFuture[List[T]] =
        WrappedFuture(Future.traverse(t)(_.future).map(_.traverse))

    }

}
