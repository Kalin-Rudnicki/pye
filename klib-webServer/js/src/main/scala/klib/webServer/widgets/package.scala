package klib.webServer

package object widgets {

  type Widget[Value] = GWidget[Value, Widget.State[Value]]

}
