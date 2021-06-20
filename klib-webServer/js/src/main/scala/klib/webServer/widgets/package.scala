package klib.webServer

package object widgets {

  type GWidgetF[Value, S] = S => GWidget[Value, S]
  type Widget[Value] = GWidget[Value, _]

}
