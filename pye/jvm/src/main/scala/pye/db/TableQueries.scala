package pye.db

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{Query => SquerylQuery, _}
import org.squeryl.dsl._
import org.squeryl.dsl.ast._

import klib.Implicits._
import klib.fp.types._

final class TableQueries[T <: DbObject] private[db] (
    table: Table[T],
)(implicit
    ked: KeyedEntityDef[T, Long],
) {

  // =====| Insert |=====

  object insert {

    def apply(t: T): Query[T] =
      table.insert(t).pure[Query]

    def all(ts: List[T]): Query[Unit] =
      table.insert(ts).pure[Query]
    def all(tN: T*): Query[Unit] =
      all(tN.toList)

  }

  // =====| Select |=====

  def select(filter: T => LogicalBoolean): TableQueries.WrappedSquerylQuery[T] =
    new TableQueries.WrappedSquerylQuery[T](
      from(table) { t => where(filter(t)).select(t) },
    )

  // =====| Update |=====

  object update {

    def apply(t: T): Query[T] =
      for {
        _ <- table.update(t).pure[Query]
      } yield t

    def all(ts: List[T]): Query[List[T]] =
      for {
        _ <- table.update(ts).pure[Query]
      } yield ts
    def all(tN: T*): Query[List[T]] =
      all(tN.toList)

    object where {

      def apply(s: T => UpdateStatement): Query[Int] =
        table.update(s).pure[Query]

      def exactly(exp: Int)(s: T => UpdateStatement): Query[Unit] =
        for {
          updated <- where(s)
          _ <-
            if (updated == exp) ().pure[Query]
            else Query(IO.error(Message(s"Expected $exp updates, but got $updated")))
        } yield ()

      def single(s: T => UpdateStatement): Query[Unit] =
        exactly(1)(s)

    }

  }

  // =====| Delete |=====

  object delete {

    def apply(t: T)(implicit ked: KeyedEntityDef[T, Long]): Query[Unit] =
      where.single(_.id === t.id)

    object where {

      def apply(clause: T => LogicalBoolean): Query[Int] =
        table.deleteWhere(clause).pure[Query]

      def exactly(exp: Int)(clause: T => LogicalBoolean): Query[Unit] =
        for {
          deleted <- where(clause)
          _ <-
            if (deleted == exp) ().pure[Query]
            else Query(IO.error(Message(s"Expected $exp deletes, but got $deleted")))
        } yield ()

      def single(clause: T => LogicalBoolean): Query[Unit] =
        exactly(1)(clause)

    }

  }

  // =====| QueryN |=====

  def query1[P1, P1T](
      p1F: T => TypedExpression[P1, P1T],
  )(implicit
      p1CC: CanCompare[P1T, P1T],
  ): TableQueries.Query1[T, P1, P1T] =
    new TableQueries.Query1[T, P1, P1T] {
      override def apply(
          p1: TypedExpression[P1, P1T],
      ): TableQueries.WrappedSquerylQuery[T] =
        select { t =>
          p1F(t) === p1
        }
    }

  def query2[P1, P1T, P2, P2T](
      p1F: T => TypedExpression[P1, P1T],
      p2F: T => TypedExpression[P2, P2T],
  )(implicit
      p1CC: CanCompare[P1T, P1T],
      p2CC: CanCompare[P2T, P2T],
  ): TableQueries.Query2[T, P1, P1T, P2, P2T] =
    new TableQueries.Query2[T, P1, P1T, P2, P2T] {
      override def apply(
          p1: TypedExpression[P1, P1T],
          p2: TypedExpression[P2, P2T],
      ): TableQueries.WrappedSquerylQuery[T] =
        select { t =>
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
  ): TableQueries.Query3[T, P1, P1T, P2, P2T, P3, P3T] =
    new TableQueries.Query3[T, P1, P1T, P2, P2T, P3, P3T] {
      override def apply(
          p1: TypedExpression[P1, P1T],
          p2: TypedExpression[P2, P2T],
          p3: TypedExpression[P3, P3T],
      ): TableQueries.WrappedSquerylQuery[T] =
        select { t =>
          p1F(t) === p1 and
            p2F(t) === p2 and
            p3F(t) === p3
        }
    }

}
object TableQueries {

  final class WrappedSquerylQuery[T] private[db] (
      query: SquerylQuery[T],
  ) {
    def single: Query[T] = query.single.pure[Query]
    def maybe: Query[Maybe[T]] = query.singleOption.toMaybe.pure[Query]
    def list: Query[List[T]] = query.toList.pure[Query]
  }

  sealed trait Query1[T, P1, P1T] {
    def apply(
        p1: TypedExpression[P1, P1T],
    ): WrappedSquerylQuery[T]
  }
  sealed trait Query2[T, P1, P1T, P2, P2T] {
    def apply(
        p1: TypedExpression[P1, P1T],
        p2: TypedExpression[P2, P2T],
    ): WrappedSquerylQuery[T]
  }
  sealed trait Query3[T, P1, P1T, P2, P2T, P3, P3T] {
    def apply(
        p1: TypedExpression[P1, P1T],
        p2: TypedExpression[P2, P2T],
        p3: TypedExpression[P3, P3T],
    ): WrappedSquerylQuery[T]
  }

}
