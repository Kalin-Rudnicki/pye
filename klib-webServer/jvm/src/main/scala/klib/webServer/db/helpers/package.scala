package klib.webServer.db

import org.squeryl.{Query => SquerylQuery, _}
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl.ast.LogicalBoolean

import klib.Implicits._
import klib.fp.types._

package object helpers {

  trait DbObject extends KeyedEntity[Long] {
    var id: Long = 0L
  }

  // =====|  |=====

  def all[T](table: Table[T]): Query[List[T]] =
    from(table) { t =>
      select(t)
    }.toList
      .pure[Query]

  // =====|  |=====

  private def lookupSquerylQuery[T](table: Table[T], filter: T => LogicalBoolean): SquerylQuery[T] =
    from(table) { t =>
      where(filter(t))
        .select(t)
    }

  def lookup[T](table: Table[T])(filter: T => LogicalBoolean): Query[T] =
    lookupSquerylQuery(table, filter).single.pure[Query]

  def lookupMaybe[T](table: Table[T])(filter: T => LogicalBoolean): Query[Maybe[T]] =
    lookupSquerylQuery(table, filter).singleOption.toMaybe.pure[Query]

  def lookupList[T](table: Table[T])(filter: T => LogicalBoolean): Query[List[T]] =
    lookupSquerylQuery(table, filter).toList.pure[Query]

  // =====|  |=====

  def lookupById[T <: DbObject](table: Table[T])(id: Long): Query[T] =
    lookup(table)(_.id === id)

  def lookupByIdMaybe[T <: DbObject](table: Table[T])(id: Long): Query[Maybe[T]] =
    lookupMaybe(table)(_.id === id)

  def lookupByIdList[T <: DbObject](table: Table[T])(id: Long): Query[List[T]] =
    lookupList(table)(_.id === id)

}
