package klib.webServer.test

import io.circe._
import io.circe.generic.auto._

object Test {
  def main(args: Array[String]): Unit = {
    println(
      implicitly[Encoder[Unit]].apply(()).toString,
    )
  }
}
