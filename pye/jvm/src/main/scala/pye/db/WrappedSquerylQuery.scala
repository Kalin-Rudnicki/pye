package pye.db

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{Query => SquerylQuery}

import klib.Implicits._
import klib.fp.types._

final class WrappedSquerylQuery[T] private[db] (
    query: SquerylQuery[T],
) {
  def single: Query[T] = query.single.pure[Query]
  def maybe: Query[Maybe[T]] = query.singleOption.toMaybe.pure[Query]
  def list: Query[List[T]] = query.toList.pure[Query]
}
