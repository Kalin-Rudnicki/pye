package pye.db

import scala.util.Try

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._

trait EnumCompanion[Data] extends Enumeration {

  def fromData(data: Data): Value
  def toData(model: Value): Data

  final def fromInt(i: Int): ?[Value] =
    Try { this(i) }.to_?

  final implicit val decodeFromString: DecodeFromString[Value] =
    DecodeFromString.intDecodeString.fMap(fromInt)

}
