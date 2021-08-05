package klib.webServer.test

import scala.concurrent.ExecutionContext.Implicits.global

import monocle.macros.GenLens
import org.scalajs.dom._
import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils._
import klib.webServer._
import klib.webServer.widget._
import klib.webServer.widget.widgets.all._

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
        person1: Person,
        person2: Person,
    )

    final case class CoupleState(
        person1: PersonState,
        person2: PersonState,
    )
    object CoupleState {
      val empty: CoupleState = CoupleState(PersonState.empty, PersonState.empty)
    }

    // =====|  |=====

    val personWidget: Widget.Submit[Person, PersonState] =
      for {
        person <-
          ado[Widget.Projection[PersonState, CommonRaise.Submit.type]#P]
            .join(
              inputW[String]().required().labeled("First name:").zoomOut(GenLens[PersonState](_.firstName)),
              Widget.builder.element(br.render),
              inputW[String]().required().labeled("Last name:").zoomOut(GenLens[PersonState](_.lastName)),
              Widget.builder.element(br.render),
              inputW[Int]().required().labeled("Age:").zoomOut(GenLens[PersonState](_.age)),
            )
            .mapValue {
              case (firstName, _, lastName, _, age) =>
                Person(firstName, lastName, age)
            }: Widget.Projection[PersonState, CommonRaise.Submit.type]#P[Person]
        _ <- Widget.builder.element {
          div(s"[${person.firstName} ${person.lastName} (${person.age})]").render
        }: Widget.Projection[PersonState, CommonRaise.Submit.type]#P[Unit]
      } yield person

    val coupleWidget: Widget.Submit[Couple, CoupleState] =
      ado[Widget.Projection[CoupleState, CommonRaise.Submit.type]#P]
        .join(
          personWidget.labeled("Person1:").zoomOut(GenLens[CoupleState](_.person1)),
          Widget.builder.element(br.render),
          personWidget.labeled("Person2:").zoomOut(GenLens[CoupleState](_.person2)),
        )
        .mapValue {
          case (person1, _, person2) =>
            Couple(person1, person2)
        }

    def sendCreateCoupleApiRequest(couple: Couple): AsyncIO[Unit] = ???

    val coupleForm: Widget.NoAction[Couple, CoupleState] = {
      ado[Widget.Projection[CoupleState, CommonRaise.Submit.type]#P]
        .join(
          Widget.builder.element(h2("--- Couple Form ---").render),
          coupleWidget,
          submitButton(),
        )
        .mapValue(_._2)
        .handleAction { (_, coupleValue, _) =>
          for {
            couple <- AsyncIO.wrapEffect(coupleValue)
            _ <- sendCreateCoupleApiRequest(couple)
          } yield List(
            Raise.UpdateState[CoupleState](_ => CoupleState.empty),
            Raise.DisplayMessage.global.info("Created couple", 2500.some),
          )
        }
    }

    val coupleDiv: Element =
      div(
        coupleForm.renderNoAction(CoupleState.empty).toList,
      ).render

  }
}
