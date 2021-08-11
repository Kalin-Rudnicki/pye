package pye.facades.raw

import scala.scalajs.js

@js.native
@js.annotation.JSGlobalScope
object sourceMap extends js.Object {

  @js.native
  object SourceMapData extends js.Object {
    val srcMap: SourceMapConsumer = js.native
    val srcText: String = js.native
    val srcLines: js.Array[String] = js.native
  }

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
