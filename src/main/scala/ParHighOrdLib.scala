
object ParHighOrdLib extends App {
  import Parallel._

  def scanLeft[T](inp: Array[T], out: Array[T])(init: T)(op: (T, T) => T): Unit = {
    val threshold = 2

    trait Tree {
      val value: T
    }
    case class Leaf(override val value: T, from: Int, to: Int) extends Tree
    case class Node(override val value: T,
                       left: Tree, right: Tree) extends Tree

    def reduceSeg(from: Int, to: Int, a0: T): T = {
      var res = a0
      for (i <- from until to) {
        res = op(res, inp(i))
      }
      res
    }

    def scanLeftSegWithShift(from: Int, to: Int, a0: T): Unit = {
      var a = a0
      for (i <- from until to) {
        a = op(a, inp(i))
        out(i + 1) = a
      }
    }

    def sweepUp(from: Int, to: Int): Tree = {
      if (to - from < threshold) {
        Leaf(reduceSeg(from + 1, to, inp(from)), from, to)
      } else {
        val mid = (to + from) >> 1
        val (lt, rt) = parallel(sweepUp(from, mid), sweepUp(mid, to))
        Node(op(lt.value, rt.value), lt, rt)
      }
    }

    def sweepDown(tree: Tree, a0: T): Unit = tree match {
      case Leaf(v, from, to) => scanLeftSegWithShift(from, to, a0)
      case Node(v, l, r) => val (_, _) = parallel(sweepDown(l, a0), sweepDown(r, op(a0, l.value)))
    }

    val t = sweepUp(0, inp.length)
    sweepDown(t, init)
    out(0) = init
  }
}