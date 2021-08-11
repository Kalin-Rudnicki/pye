package pye.facades.wrappers

import scala.scalajs.js

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils._

import pye.facades

object sourceMap {

  final class SourceMapConsumer private (raw: facades.raw.sourceMap.SourceMapConsumer) {

    def originalPositionFor(
        line: Int,
        column: Int = 0,
        bias: SourceMapConsumer.Bias = SourceMapConsumer.Bias.LeastUpperBound,
    ): Maybe[SourceMapConsumer.OriginalPosition] = {
      val rawRes =
        raw.originalPositionFor(
          js.Dictionary(
            "line" -> line,
            "column" -> column,
            "bias" -> bias.raw,
          ),
        )

      ado[Maybe]
        .join(
          Maybe(rawRes.source),
          Maybe(rawRes.line),
          Maybe(rawRes.column),
        )
        .map {
          case (source, line, column) =>
            SourceMapConsumer.OriginalPosition(
              source = source,
              line = line,
              column = column,
              name = Maybe(rawRes.name),
            )
        }
    }

    def destroy(): Unit =
      raw.destroy()

  }
  object SourceMapConsumer {

    sealed abstract class Bias(private[SourceMapConsumer] val raw: js.Any)
    object Bias {
      case object GreatestLowerBound extends Bias(js.Dynamic.global.sourceMap.SourceMapConsumer.GREATEST_LOWER_BOUND)
      case object LeastUpperBound extends Bias(js.Dynamic.global.sourceMap.SourceMapConsumer.LEAST_UPPER_BOUND)
    }

    final case class OriginalPosition(
        source: String,
        line: Int,
        column: Int,
        name: Maybe[String],
    )

    def callNew(arg: js.Any): AsyncIO[SourceMapConsumer] = {
      val jsPromise: js.Promise[facades.raw.sourceMap.SourceMapConsumer] =
        js.Dynamic.global.sourceMap.SourceMapConsumer
          .callNew(arg)
          .asInstanceOf[js.Promise[facades.raw.sourceMap.SourceMapConsumer]]

      for {
        rawConsumer <- AsyncIO.wrapFuture(_ => jsPromise.toFuture)
      } yield new SourceMapConsumer(rawConsumer)
    }

  }

}
