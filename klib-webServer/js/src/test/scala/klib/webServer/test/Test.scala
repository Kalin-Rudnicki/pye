package klib.webServer.test

import scala.concurrent.ExecutionContext.Implicits.global
import monocle.syntax.all._
import monocle.macros.GenLens
import monocle._
import org.scalajs.dom._
import scalatags.JsDom.all._
import klib.Implicits._
import klib.fp.types._
import klib.webServer._
import klib.webServer.widget._
import klib.webServer.widget.Widget.Raise._

object Test {
  def main(args: Array[String]): Unit = {

    // =====|  |=====

    final case class Person(
        firstName: String,
        lastName: String,
        age: Int,
    ) {
      override def toString: String = s"Person($firstName $lastName ($age))"
    }

    final case class PersonState(
        firstName: String,
        lastName: String,
        age: String,
    )
    object PersonState {
      val empty: PersonState = PersonState("", "", "")
    }

    final case class Couple(
        man: Person,
        woman: Person,
    )

    final case class CoupleState(
        man: PersonState,
        woman: PersonState,
    )
    object CoupleState {
      val empty: CoupleState = CoupleState(PersonState.empty, PersonState.empty)
    }

    // =====|  |=====

    // val personWidget: Widget[Person, PersonState, SubmitOr[Nothing]]
    val personWidget: Widget.StdForm[Person, PersonState] =
      ado[_]
        .join(
          inputs.input[String]().required().labeled("First name:").zoomOut(GenLens[PersonState](_.firstName)),
          Widget.element(br.render),
          inputs.input[String]().required().labeled("Last name:").zoomOut(GenLens[PersonState](_.lastName)),
          Widget.element(br.render),
          inputs.input[Int]().required().labeled("Age:").zoomOut(GenLens[PersonState](_.age)),
        )
        .mapValue {
          case (firstName, _, lastName, _, age) =>
            Person(firstName, lastName, age)
        }

    val coupleWidget: Widget.StdForm[Couple, CoupleState] =
      ado[_]
        .join(
          personWidget.labeled("Man:").zoomOut(GenLens[CoupleState](_.man)),
          Widget.element(br.render),
          personWidget.labeled("Woman:").zoomOut(GenLens[CoupleState](_.woman)),
        )
        .mapValue {
          case (man, _, woman) =>
            Couple(man, woman)
        }

    val coupleForm: Widget.NoAction[Couple, CoupleState] = {
      ado[_]
        .join(
          Widget.builder
            .withState[CoupleState]
            .submitAction
            .element(h2("--- Couple Form ---").render),
          coupleWidget,
          submitButton(),
        )
        .mapValue(_._2)
        .handleAction { (coupleState, coupleValue, action) => // would always be `case object Submit`
          for {
            couple <- WrappedFuture.wrap_?(coupleValue)
            _ <- Api.couple.create(couple)
          } yield List(
            UpdateState[CoupleState](_ => CoupleState.empty),
            DisplayMessage.global.info("Created couple", 2500.some),
          )
        }
    }

  }
}
