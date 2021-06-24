package klib.webServer.test

import java.util.UUID

object Test {

  def main(args: Array[String]): Unit = {
    val padTo = 35

    sealed trait Node {

      def toList: List[Int] =
        this match {
          case Node.Leaf(i) =>
            i :: Nil
          case Node.Branch(_1, _2) =>
            _1.toList ::: _2.toList
        }

      override def toString: String =
        this match {
          case Node.Leaf(i) =>
            i.toString
          case Node.Branch(_1, _2) =>
            s"[${_1}, ${_2}]"
        }

    }

    object Node {
      final case class Leaf(i: Int) extends Node
      final case class Branch(_1: Node, _2: Node) extends Node
    }

    final case class MyWrap[T](t: T, node: Node) {

      def map[T2](f: T => T2): MyWrap[T2] = {
        val uuid = UUID.randomUUID
        println
        println(s"${this.toString.padTo(padTo, ' ')}.    map ($uuid)  = ")
        val res = f(t)
        println(s"${this.toString.padTo(padTo, ' ')}.    map ($uuid)  = $res")
        MyWrap(res, node)
      }

      def flatMap[T2](f: T => MyWrap[T2]): MyWrap[T2] = {
        val uuid = UUID.randomUUID
        println
        println(s"${this.toString.padTo(padTo, ' ')}.flatMap ($uuid) <- ")
        val res = f(t)
        println(s"${this.toString.padTo(padTo, ' ')}.flatMap ($uuid) <- $res")
        MyWrap(res.t, Node.Branch(node, res.node))
      }

    }

    def test(
        i: Int,
        mR: => MyWrap[_],
    ): Unit = {
      println
      println(s"--- $i ---")
      val r = mR
      println(r)
      println(r.node.toList)
    }

    test(
      1,
      for {
        i1 <- MyWrap(2, Node.Leaf(1))
        j1 <- MyWrap(3, Node.Leaf(2))
        ij1 <- MyWrap((i1, j1), Node.Leaf(3))

        _ = 12

        _ <- MyWrap(8, Node.Leaf(4))

        ((i2, j2), _) = (ij1, ())
        _ = 7
      } yield i2 * j2,
    )

    test(
      2,
      for {
        tmp1 <- for {
          _1 <- MyWrap("A", Node.Leaf(1))
          _2 <- MyWrap("B", Node.Leaf(2))
          _3 <- MyWrap("C", Node.Leaf(3))
        } yield (_1, _2, _3)
        (_1, _2, _3) = tmp1

        tmp2 <- for {
          _4 <- MyWrap("D", Node.Leaf(4))
          _5 <- MyWrap("E", Node.Leaf(5))
          _6 <- MyWrap("F", Node.Leaf(6))
        } yield (_4, _5, _6)
        (_4, _5, _6) = tmp2
      } yield (_1, _2, _3, _4, _5, _6),
    )

    /*
    println
    println("Raw:")
    MyWrap(2, 1 :: Nil).flatMap { i1 =>
      MyWrap(3, 2 :: Nil).flatMap { i2 =>
        MyWrap((i1, i2), 3 :: Nil).map { ij1 =>
          (ij1, 12)
        }.flatMap { ij1_12 =>
          MyWrap(8, 4 :: Nil).map { _ =>

          }
        }
      }
    }
     */

  }

}
