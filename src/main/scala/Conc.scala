import scala.annotation.tailrec

sealed trait Conc[+T] {
  def height: Int
  def size: Int
  def left: Conc[T] = Empty
  def right: NormalConc[T] = Empty
  def concat[U >: T](that: Conc[U]): Conc[U] = {
    Conc.concat(this, that)
  }

  def appendLeaf[U >: T](that: Singleton[U]): Conc[U] = {
    Conc.appendLeaf(this, that)
  }

  def append[U >: T](value: U): Conc[U] = {
    Conc.append(this, value)
  }

  def normalize: NormalConc[T] = {
    Conc.normalize(this)
  }
}

sealed trait NormalConc[+T] extends Conc[T] {
  override val left: NormalConc[T] = Empty
}

object Conc {
  def empty[T]: Conc[T] = Empty

  def singleton[T](value: T): Conc[T] = Singleton(value)

  def apply[T](): Conc[T] = empty

  def apply[T](value: T): Conc[T] = singleton(value)

  def apply[T](x: Conc[T], y: Conc[T]): Conc[T] = Conc(x, y)

  def concatNormal[T](x: NormalConc[T], y: NormalConc[T]): NormalConc[T] = {
    val diff = x.height - y.height
    if (-1 <= diff && diff <= 1) {
      <>(x, y)
    } else if (diff > 1) {
      if (x.left.height >= x.right.height) {
        val nr = concatNormal(x.right, y)
        <>(x.left, nr)
      } else {
        val z = concatNormal(x.right.right, y)
        if (z.height == x.height - 3) {
          val nr = <>(x.right.left, z)
          concatNormal(x.left, nr)
        } else {
          val nl = concatNormal(x.left, x.right.left)
          <>(nl, z)
        }
      }
    } else {
      if (y.left.height <= x.right.height) {
        val nl = concatNormal(x, y.left)
        <>(nl, y.right)
      } else {
        val z = concatNormal(x, y.left.left)
        if (z.height == y.height - 3) {
          val nl = <>(z, y.left.right)
          <>(nl, y.right)
        } else {
          val nr = <>(y.left.right, y.right)
          <>(z, nr)
        }
      }
    }
  }

  def concat[T](left: Conc[T], right: Conc[T]): Conc[T] = {
    val (x, y) = (left.normalize, right.normalize)
    concatNormal(x, y)
  }

  @tailrec
  def normalize[T](tree: Conc[T]): NormalConc[T] = {
    tree match {
      case tree: NormalConc[T] => tree
      case tree@ Append(Append(ll, lr), r) => normalize(Append(ll, concatNormal(lr, r)))
      case tree@ Append(l: NormalConc[T], r) => concatNormal(l, r)
    }
  }

  def append[T](tree: Conc[T], value: T): Conc[T] = {
    appendLeaf(tree, Singleton(value))
  }

  def appendLeaf[T](tree: Conc[T], leaf: Singleton[T]): Conc[T] = {
    @tailrec
    def loop(x: Append[T], y: NormalConc[T]): Conc[T] = {
      if (x.right.height > y.height) {
        Append(x, y)
      } else {
        assert(x.right.height == y.height)
        val z = <>(x.right, y)
        x.left match {
          case xl: Append[T] => loop(xl, z)
          case xl: NormalConc[T] if xl.height == z.height => <>(xl, z)
          case xl: NormalConc[T] => Append(xl, z)
        }
      }
    }
    tree match {
      case Empty => leaf
      case x: Singleton[T] => <>(x, leaf)
      case x: <>[T] => Append(x, leaf)
      case x: Append[T] => loop(x, leaf)
    }
  }
}

object Empty extends NormalConc[Nothing] {
  override val height: Int = 0
  override val size: Int = 0
}

case class Singleton[+T](value: T) extends NormalConc[T] {
  override val height: Int = 0
  override val size: Int = 1
}

case class <>[+T](override val left: NormalConc[T], override val right: NormalConc[T]) extends NormalConc[T] {
  assert(
    math.abs(left.height - right.height) < 2,
    "Difference between the height of the" + "left and right subtree of <> must be less than 2")

  override val height: Int = math.max(left.height, right.height) + 1
  override val size: Int = left.size + right.size
}

case class Append[+T](override val left: Conc[T], override val right: NormalConc[T]) extends Conc[T] {
  override val height: Int = math.max(left.height, right.height) + 1
  override val size: Int = left.size + right.size
}