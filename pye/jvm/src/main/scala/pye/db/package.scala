package pye

import org.squeryl.{Query => _, _}

import klib.Implicits._
import klib.fp.types._

package object db {

  implicit class MQueryOps[T](query: Query[Maybe[T]]) {

    def orError(message: String): Query[T] =
      query.flatMap {
        case Some(a) => a.pure[Query]
        case None    => Query(IO.error(Message(message)))
      }

  }

  implicit class TableOps[T <: DbObject](table: Table[T]) {

    def queries(implicit ked: KeyedEntityDef[T, Long]): TableQueries[T] = new TableQueries[T](table)

  }

}
