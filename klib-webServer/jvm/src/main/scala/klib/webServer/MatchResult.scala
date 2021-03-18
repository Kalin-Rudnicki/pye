package klib.webServer

import klib.fp.typeclass._

// =====|  |=====

sealed trait MatchResult[+T]
object MatchResult {

  final case class Continue[+T](value: T) extends MatchResult[T]
  final case class Done(result: Response) extends MatchResult[Nothing]

  //

  implicit val matchResultMonad: Monad[MatchResult] =
    new Monad[MatchResult] {

      override def map[A, B](t: MatchResult[A], f: A => B): MatchResult[B] =
        t match {
          case Continue(value) =>
            Continue(f(value))
          case done @ Done(_) =>
            done
        }

      // NOTE : Don't really like the whole apply thing, but want map & flatMap
      override def apply[A, B](t: MatchResult[A], f: MatchResult[A => B]): MatchResult[B] =
        t match {
          case Continue(tValue) =>
            f match {
              case Continue(fValue) =>
                Continue(fValue(tValue))
              case done @ Done(_) =>
                done
            }
          case done @ Done(_) =>
            done
        }

      override def pure[A](a: => A): MatchResult[A] =
        Continue(a)

      override def flatten[A](t: MatchResult[MatchResult[A]]): MatchResult[A] =
        t match {
          case Continue(value) =>
            value
          case done @ Done(_) =>
            done
        }

    }

}
