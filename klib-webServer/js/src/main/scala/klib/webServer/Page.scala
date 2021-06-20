package klib.webServer

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalajs.dom._
import scalatags.JsDom
import klib.Implicits._
import klib.fp.types.{Alive, Dead}

trait Page {

  // =====|  |=====
  val pageTitle: String
  def pageBody: HttpResponse[JsDom.TypedTag[html.Body]]

  // =====|  |=====
  def render(): Unit = {
    pageBody.future.onComplete {
      _.to_?.flatten match {
        case Alive(r) =>
          document.title = pageTitle
          document.body = r.render
        case Dead(errors) =>
          console.log("Error loading page:")
          errors.foreach(console.log(_))
      }
    }
  }

}
