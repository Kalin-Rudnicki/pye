package klib.webServer

package object widgets {

  trait all extends containers with events with inputs with widgets.Implicits
  object all extends all

}
