package klib.webServer.widgets

import org.scalajs.dom.Node
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._

final class Widget[+V] private (
    val node: Node,
    _value: => ?[V],
    private val errorMappings: List[Throwable => Throwable],
) {

  private def rawValue: ?[V] = _value

  private def mapError(throwable: Throwable): Throwable =
    errorMappings.foldLeft(throwable)((t, f) => f(t))

  def value: ?[V] =
    rawValue.mapErrors(mapError)

}
object Widget {

  def apply[V](node: Node, value: => ?[V], errorMappings: List[Throwable => Throwable] = Nil): Widget[V] =
    new Widget[V](node, value, errorMappings)

  implicit val widgetApplicative: Applicative[Widget] =
    new Applicative[Widget] {

      override def map[A, B](t: Widget[A], f: A => B): Widget[B] =
        new Widget[B](
          node = t.node,
          _value = t.rawValue.map(f),
          t.errorMappings,
        )

      override def apply[A, B](t: Widget[A], f: Widget[A => B]): Widget[B] =
        new Widget[B](
          node = List(t.node, f.node).render, // TODO (KR) : Maybe switch?
          _value = t.value.apply(f.value),
          errorMappings = Nil,
        )

      override def pure[A](a: => A): Widget[A] =
        new Widget[A](
          node = List.empty[Node].render,
          _value = a.pure[?],
          errorMappings = Nil,
        )

    }

  implicit val widgetTraverseList: Traverse[List, Widget] =
    new Traverse[List, Widget] {

      override def traverse[T](t: List[Widget[T]]): Widget[List[T]] =
        Widget(
          t.map(_.node).render,
          t.map(_.value).traverse,
          Nil,
        )

    }
  implicit val widgetTraverseNonEmptyList: Traverse[NonEmptyList, Widget] =
    new Traverse[NonEmptyList, Widget] {

      override def traverse[T](t: NonEmptyList[Widget[T]]): Widget[NonEmptyList[T]] =
        Widget(
          t.map(_.node).toList.render,
          t.map(_.value).traverse,
          Nil,
        )

    }

  final class Builder[+V, -S] private[Widget] (private val build: S => Widget[V]) {

    def apply(s: S): Widget[V] =
      build(s)

    def mapNode(f: Node => Node): Builder[V, S] =
      new Builder[V, S](s => {
        val w = build(s)
        new Widget[V](f(w.node), w.rawValue, w.errorMappings)
      })

    // NOTE : If using this, it is important that anything that is actually looking for changes in S2's state,
    //      : then S & S2 need to have the same reference to that.
    //      :
    //      : Ex: S1(var value: String)
    //      :     S2(var value: String)
    //      :     val s1 = S1("1")
    //      :     val s2 = S2(s1.value)
    //      :     s2.value = "2"
    //      ;     s1.value // => "1"
    //      ;     [BAD]
    //      :
    //      : Ex: S3(state: Var[String])
    //      :     S4(state: Var[String])
    //      :     val s3 = S3(Var("3"))
    //      :     val s4 = S4(s3.state)
    //      :     s4.state.value = "4"
    //      ;     s3.value // => "4"
    //      ;     [GOOD]
    def rMapState[S2](sF: S2 => S): Builder[V, S2] =
      new Builder[V, S2](s => build(sF(s)))

    def mapErrors(f: Throwable => Throwable): Builder[V, S] =
      new Builder[V, S](s => {
        val w = build(s)
        new Widget[V](
          w.node,
          w.rawValue,
          f :: w.errorMappings,
        )
      })

    def labelErrors(label: String): Builder[V, S] =
      mapErrors(t => new Throwable(s"$label${t.getMessage}", t))

    def flatMapValue[V2](f: V => ?[V2]): Builder[V2, S] =
      new Builder[V2, S](s => {
        val w = build(s)
        new Widget[V2](
          w.node,
          w.rawValue.flatMap(f),
          w.errorMappings,
        )
      })

  }
  object Builder {

    def apply[V, S](build: S => Widget[V]): Builder[V, S] =
      new Builder[V, S](build)

    def node(n: => Node): Builder[Unit, Any] =
      new Builder[Unit, Any](_ => new Widget[Unit](n, ().pure[?], Nil))

    // =====|  |=====

    type Projection[S] = { type P[V] = Builder[V, S] }

    implicit def builderApplicative[S]: Applicative[Projection[S]#P] =
      new Applicative[Projection[S]#P] {

        override def map[A, B](t: Builder[A, S], f: A => B): Builder[B, S] =
          new Builder[B, S](t.build(_).map(f))

        override def apply[A, B](t: Builder[A, S], f: Builder[A => B, S]): Builder[B, S] =
          new Builder[B, S](s => t.build(s).apply(f.build(s)))

        override def pure[A](a: => A): Builder[A, S] =
          new Builder[A, S](_ => a.pure[Widget])

      }

  }

}
