package pye.db

import org.squeryl.KeyedEntity

trait DbObject extends KeyedEntity[Long] {
  var id: Long = 0L
}
