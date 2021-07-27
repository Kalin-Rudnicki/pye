package klib.webServer

import scala.concurrent.{ExecutionContext, Future, Promise}
import klib.Implicits._
import klib.fp.typeclass.Monad
import klib.fp.types._

final class HttpResponse[+T] private (val future: Future[?[T]]) {

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
object HttpResponse {

  def apply[T](future: Future[?[T]]): HttpResponse[T] =
    new HttpResponse[T](future)

  implicit def httpResponseMonad(implicit ec: ExecutionContext): Monad[HttpResponse] =
    new Monad[HttpResponse] {

      override def map[A, B](t: HttpResponse[A], f: A => B): HttpResponse[B] = {
        val p: Promise[?[B]] = Promise()
        t.future.onComplete { t =>
          p.success(t.to_?.flatten.map(f))
        }

        new HttpResponse[B](p.future)
      }

      override def apply[A, B](t: HttpResponse[A], f: HttpResponse[A => B]): HttpResponse[B] = {
        val p: Promise[?[B]] = Promise()

        f.future.onComplete { fT =>
          t.future.onComplete { tT =>
            val f_? = fT.to_?.flatten
            val t_? = tT.to_?.flatten

            p.success(t_?.apply(f_?))
          }
        }

        new HttpResponse[B](p.future)
      }

      override def pure[A](a: => A): HttpResponse[A] =
        new HttpResponse[A](Future(a.pure[?]))

      override def flatten[A](t: HttpResponse[HttpResponse[A]]): HttpResponse[A] = {
        val p: Promise[?[A]] = Promise()
        t.future.onComplete { t1 =>
          t1.to_?.flatten match {
            case Alive(r) =>
              r.future.onComplete { t2 =>
                p.success(t2.to_?.flatten)
              }
            case dead @ Dead(_) =>
              p.success(dead)
          }
        }

        new HttpResponse[A](p.future)
      }

    }

}
