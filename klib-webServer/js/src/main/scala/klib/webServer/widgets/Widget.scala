package klib.webServer.widgets

import scala.concurrent.ExecutionContext

import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._
import klib.webServer._

final class Widget[+V] private (
    val render: () => Node,
    _value: => ?[V],
    private val errorMappings: List[Throwable => Throwable],
) {
  private var _node: Node = render()
  def node: Node = _node

  private def rawValue: ?[V] = _value

  private def mapError(throwable: Throwable): Throwable =
    errorMappings.foldLeft(throwable)((t, f) => f(t))

  def value: ?[V] =
    rawValue.mapErrors(mapError)

  def reRender(): Unit = {
    // REMOVE : ...
    // console.log("reRender()")

    val newNode = render()
    _node.parentNode.replaceChild(newNode, _node)
    _node = newNode
  }

}

object Widget {

  def apply[V](
      value: => ?[V],
      errorMappings: List[Throwable => Throwable] = Nil,
  )(
      node: => Node,
  ): Widget[V] =
    new Widget[V](() => node, value, errorMappings)

  implicit val widgetApplicative: Applicative[Widget] =
    new Applicative[Widget] {

      override def map[A, B](t: Widget[A], f: A => B): Widget[B] =
        new Widget[B](
          render = t.render,
          _value = t.rawValue.map(f),
          t.errorMappings,
        )

      override def apply[A, B](t: Widget[A], f: Widget[A => B]): Widget[B] =
        new Widget[B](
          render = () => List(t.render(), f.render()).render,
          _value = t.value.apply(f.value),
          errorMappings = Nil,
        )

      override def pure[A](a: => A): Widget[A] =
        new Widget[A](
          render = () => List.empty[Node].render,
          _value = a.pure[?],
          errorMappings = Nil,
        )

    }

  implicit val widgetTraverseList: Traverse[List, Widget] =
    new Traverse[List, Widget] {

      override def traverse[T](t: List[Widget[T]]): Widget[List[T]] =
        new Widget(
          () => t.map(_.render()).render,
          t.map(_.value).traverse,
          Nil,
        )

    }
  implicit val widgetTraverseNonEmptyList: Traverse[NonEmptyList, Widget] =
    new Traverse[NonEmptyList, Widget] {

      override def traverse[T](t: NonEmptyList[Widget[T]]): Widget[NonEmptyList[T]] =
        new Widget(
          () => t.map(_.reRender()).toList.render,
          t.map(_.value).traverse,
          Nil,
        )

    }

  final class Builder[+V, -S] private[Widget] (private val build: S => Widget[V]) {

    def apply(s: S): Widget[V] = {
      val w = build(s)
      // console.log("build")
      // console.log(w.node)

      w
    }

    def mapNode(f: Node => Node): Builder[V, S] =
      new Builder[V, S](s => {
        val w = build(s)
        new Widget[V](() => f(w.render()), w.rawValue, w.errorMappings)
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
          w.render,
          w.rawValue,
          f :: w.errorMappings,
        )
      })

    def labelErrors(label: String): Builder[V, S] =
      mapErrors(t => new Throwable(s"$label${t.getMessage}", t))

    def mapValue[V2](f: V => V2): Builder[V2, S] =
      new Builder[V2, S](s => {
        val w = build(s)
        new Widget[V2](
          w.render,
          w.rawValue.map(f),
          w.errorMappings,
        )
      })

    def flatMapValue[V2](f: V => ?[V2]): Builder[V2, S] =
      new Builder[V2, S](s => {
        val w = build(s)
        new Widget[V2](
          w.render,
          w.rawValue.flatMap(f),
          w.errorMappings,
        )
      })

    def wrapInForm[R](
        endpoint: V => WrappedFuture[R],
        submitButtonLabel: String = "Submit",
        decorators: containers.FormDecorators = containers.FormDecorators(),
    )(
        onSuccess: R => Unit,
    )(implicit ec: ExecutionContext, errorHandler: ErrorHandler): Builder[V, S] =
      containers.form(
        this,
        endpoint,
        submitButtonLabel,
        decorators,
      )(onSuccess)

  }
  object Builder {

    def apply[V, S](build: S => Widget[V]): Builder[V, S] =
      new Builder[V, S](build)

    def node(n: => Node): Builder[Unit, Any] =
      new Builder[Unit, Any](_ => new Widget[Unit](() => n, ().pure[?], Nil))

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
