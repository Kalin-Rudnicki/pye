package pye.facades.raw

import scala.scalajs.js

object sourceMap {

  @js.native
  trait SourceMapConsumer extends js.Object {
    def originalPositionFor(arg: js.Any): OriginalPosition
    def destroy(): Unit
  }

  @js.native
  trait OriginalPosition extends js.Object {
    val source: String
    val line: Int
    val column: Int
    val name: String
  }

}
