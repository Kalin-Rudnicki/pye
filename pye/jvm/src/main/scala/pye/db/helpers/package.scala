package pye.db

import org.squeryl.{Query => SquerylQuery, _}
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl.{CanCompare, TypedExpression}
import org.squeryl.dsl.ast.LogicalBoolean

import klib.Implicits._
import klib.fp.types._

package object helpers {

  trait DbObject extends KeyedEntity[Long] {
    var id: Long = 0L
  }

  // =====|  |=====

  def insert[T](table: Table[T])(t: T): Query[T] =
    table.insert(t).pure[Query]

  def delete[T <: DbObject](table: Table[T])(t: T): Query[Boolean] =
    (table.deleteWhere(_.id === t.id) > 0).pure[Query]

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

  // =====|  |=====

  def listThroughJoin[T1 <: DbObject, T2 <: DbObject](
      table1: Table[T1],
      table2: Table[T2],
  )(
      t0Id: Long,
      t1_t0Id: T1 => Long,
      t1_t2Id: T1 => Long,
  ): Query[List[T2]] =
    from(table1, table2) { (t1, t2) =>
      where(t1_t0Id(t1) === t0Id and t1_t2Id(t1) === t2.id)
        .select(t2)
    }.toList
      .pure[Query]

  def listThroughJoinWExtras[T1 <: DbObject, T2 <: DbObject, E](
      table1: Table[T1],
      table2: Table[T2],
      extras: T1 => E,
  )(
      t0Id: Long,
      t1_t0Id: T1 => Long,
      t1_t2Id: T1 => Long,
  ): Query[List[(E, T2)]] =
    from(table1, table2) { (t1, t2) =>
      where(t1_t0Id(t1) === t0Id and t1_t2Id(t1) === t2.id)
        .select((extras(t1), t2))
    }.toList
      .pure[Query]

  // =====|  |=====

  implicit class TableOps[T](table: Table[T]) {

    def query1[P1, P1T](
        p1F: T => TypedExpression[P1, P1T],
    )(implicit
        p1CC: CanCompare[P1T, P1T],
    ): Query1[T, P1, P1T] =
      new Query1[T, P1, P1T] {
        override def find(
            p1: TypedExpression[P1, P1T],
        ): Query[T] =
          lookup(table) { t =>
            p1F(t) === p1
          }
        override def findM(
            p1: TypedExpression[P1, P1T],
        ): Query[Maybe[T]] =
          lookupMaybe(table) { t =>
            p1F(t) === p1
          }
        override def findL(
            p1: TypedExpression[P1, P1T],
        ): Query[List[T]] =
          lookupList(table) { t =>
            p1F(t) === p1
          }
      }

    def query2[P1, P1T, P2, P2T](
        p1F: T => TypedExpression[P1, P1T],
        p2F: T => TypedExpression[P2, P2T],
    )(implicit
        p1CC: CanCompare[P1T, P1T],
        p2CC: CanCompare[P2T, P2T],
    ): Query2[T, P1, P1T, P2, P2T] =
      new Query2[T, P1, P1T, P2, P2T] {
        override def find(
            p1: TypedExpression[P1, P1T],
            p2: TypedExpression[P2, P2T],
        ): Query[T] =
          lookup(table) { t =>
            p1F(t) === p1 and
              p2F(t) === p2
          }
        override def findM(
            p1: TypedExpression[P1, P1T],
            p2: TypedExpression[P2, P2T],
        ): Query[Maybe[T]] =
          lookupMaybe(table) { t =>
            p1F(t) === p1 and
              p2F(t) === p2
          }
        override def findL(
            p1: TypedExpression[P1, P1T],
            p2: TypedExpression[P2, P2T],
        ): Query[List[T]] =
          lookupList(table) { t =>
            p1F(t) === p1 and
              p2F(t) === p2
          }
      }

    def query3[P1, P1T, P2, P2T, P3, P3T](
        p1F: T => TypedExpression[P1, P1T],
        p2F: T => TypedExpression[P2, P2T],
        p3F: T => TypedExpression[P3, P3T],
    )(implicit
        p1CC: CanCompare[P1T, P1T],
        p2CC: CanCompare[P2T, P2T],
        p3CC: CanCompare[P3T, P3T],
    ): Query3[T, P1, P1T, P2, P2T, P3, P3T] =
      new Query3[T, P1, P1T, P2, P2T, P3, P3T] {
        override def find(
            p1: TypedExpression[P1, P1T],
            p2: TypedExpression[P2, P2T],
            p3: TypedExpression[P3, P3T],
        ): Query[T] =
          lookup(table) { t =>
            p1F(t) === p1 and
              p2F(t) === p2 and
              p3F(t) === p3
          }
        override def findM(
            p1: TypedExpression[P1, P1T],
            p2: TypedExpression[P2, P2T],
            p3: TypedExpression[P3, P3T],
        ): Query[Maybe[T]] =
          lookupMaybe(table) { t =>
            p1F(t) === p1 and
              p2F(t) === p2 and
              p3F(t) === p3
          }
        override def findL(
            p1: TypedExpression[P1, P1T],
            p2: TypedExpression[P2, P2T],
            p3: TypedExpression[P3, P3T],
        ): Query[List[T]] =
          lookupList(table) { t =>
            p1F(t) === p1 and
              p2F(t) === p2 and
              p3F(t) === p3
          }
      }

  }

  sealed trait Query1[T, P1, P1T] {
    def find(
        p1: TypedExpression[P1, P1T],
    ): Query[T]
    def findM(
        p1: TypedExpression[P1, P1T],
    ): Query[Maybe[T]]
    def findL(
        p1: TypedExpression[P1, P1T],
    ): Query[List[T]]
  }
  sealed trait Query2[T, P1, P1T, P2, P2T] {
    def find(
        p1: TypedExpression[P1, P1T],
        p2: TypedExpression[P2, P2T],
    ): Query[T]
    def findM(
        p1: TypedExpression[P1, P1T],
        p2: TypedExpression[P2, P2T],
    ): Query[Maybe[T]]
    def findL(
        p1: TypedExpression[P1, P1T],
        p2: TypedExpression[P2, P2T],
    ): Query[List[T]]
  }
  sealed trait Query3[T, P1, P1T, P2, P2T, P3, P3T] {
    def find(
        p1: TypedExpression[P1, P1T],
        p2: TypedExpression[P2, P2T],
        p3: TypedExpression[P3, P3T],
    ): Query[T]
    def findM(
        p1: TypedExpression[P1, P1T],
        p2: TypedExpression[P2, P2T],
        p3: TypedExpression[P3, P3T],
    ): Query[Maybe[T]]
    def findL(
        p1: TypedExpression[P1, P1T],
        p2: TypedExpression[P2, P2T],
        p3: TypedExpression[P3, P3T],
    ): Query[List[T]]
  }

}
