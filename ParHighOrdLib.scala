object ParHighOrdLib extends App {
  import scala.concurrent._
  import scala.concurrent.duration.Duration
  import scala.concurrent.ExecutionContext.Implicits.global

  def parallel[T1, T2](left: => T1, right: => T2): (T1, T2) = {
    val (f1, f2) = (Future(left), Future(right))
    val r1 = Await.result(f1, Duration.Inf)
    val r2 = Await.result(f2, Duration.Inf)
    (r1, r2)
  }

  def scanLeft[T](inp: Array[T], out: Array[T])(init: T)(op: (T, T) => T): Unit = {
    val threshold = 2

    trait Tree[T] {
      val value: T
    }
    case class Leaf[T](override val value: T, from: Int, to: Int) extends Tree[T]
    case class Node[T](override val value: T,
                       left: Tree[T], right: Tree[T]) extends Tree[T]

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

    def sweepUp(from: Int, to: Int): Tree[T] = {
      if (to - from < threshold) {
        Leaf(reduceSeg(from + 1, to, inp(from)), from, to)
      } else {
        val mid = (to + from) >> 1
        val (lt, rt) = parallel(sweepUp(from, mid), sweepUp(mid, to))
        Node(op(lt.value, rt.value), lt, rt)
      }
    }

    def sweepDown(tree: Tree[T], a0: T): Unit = tree match {
      case Leaf(v, f, t) => scanLeftSegWithShift(f, t, a0)
      case Node(v, l, r) => {
        val (_, _) = parallel(sweepDown(l, a0), sweepDown(r, op(a0, l.value)))
      }
    }

    val t = sweepUp(0, inp.length)
    sweepDown(t, init)
    out(0) = init
  }
}