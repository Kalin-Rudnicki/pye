package klib.webServer

import org.scalajs.dom._
import scalatags.JsDom

trait Page {

  // =====|  |=====
  val pageTitle: String
  def pageBody: JsDom.TypedTag[html.Body]

  // =====|  |=====
  def render(): Unit = {
    document.title = pageTitle
    document.body = pageBody.render
  }

}
