package klib.webServer

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import org.scalajs.dom._
import scalatags.JsDom

trait Page {

  // =====|  |=====
  val pageTitle: String
  def pageBody: Future[JsDom.TypedTag[html.Body]]

  // =====|  |=====
  def render(): Unit = {
    pageBody.onComplete {
      case Failure(exception) =>
        window.alert(s"Failed to load page ($pageTitle):\n${exception.getMessage}")
      case Success(value) =>
        document.title = pageTitle
        document.body = value.render
    }
  }

}
