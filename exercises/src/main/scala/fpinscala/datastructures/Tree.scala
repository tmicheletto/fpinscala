package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def fold[A,B](t: Tree[A])(z: A => B)(b: (B,B) => B): B = t match {
    case Leaf(v) => z(v)
    case Branch(l,r) => b((fold(l)(z)(b)),(fold(r)(z)(b)))
  }

  def size[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximum(t: Tree[Int]): Int =
    fold(t)(a => a)((l,r) => l max r)

  def depth[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((l,r) => 1 + (l max r))

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}