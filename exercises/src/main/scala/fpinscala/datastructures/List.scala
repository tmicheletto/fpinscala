package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(5,2,4) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  //foldRight(Cons(1,Cons(2,Cons(3,Nil))), Cons(4, Cons(5, Nil)))(Cons(_,_))
  //Cons(1, foldRight(Cons(2, Cons(3, Nil)), Cons(4, Cons(5, Nil))))(Cons(_,_))
  //Cons(1, Cons(2, foldRight(Cons(3, Nil), Cons(4, Cons(5, Nil)))))(Cons(_,_))
  //Cons(1, Cons(2, Cons(3, foldRight(Nil, Cons(4, Cons(5, Nil)))))(Cons(_,_))
  //Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))(Cons(_,_))

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((acc,_) => acc + 1)


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, t) => t
    }
  }


  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Cons(_,t) => Cons(h,t)
    }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case  Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }


  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t,f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_,acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }
  //foldLeft(List(1,2,3,4,5), List())(f)
  //foldLeft(List(2,3,4,5), Cons(1, List())(f)
  //foldLeft(List(3,4,5), Cons(2, Cons(1, List()))(f)
  //foldLeft(List(4,5), Cons(3, Cons(2, Cons(1, List())))(f)
  //foldLeft(List(5), Cons(4, Cons(3, Cons(2, Cons(1, List()))))(f)
  //foldLeft(Nil, Cons(5, Cons(4, Cons(3, Cons(2, Cons(1, List())))))(f)
  // Cons(2, )

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h,t) => Cons(f(h), t))

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc,h) => Cons(h,acc))

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldLeft(reverse(l), z)((b,a) => f(b,a))
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_,_))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]())(appendViaFoldRight)

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h,t) => Cons(h.toString, t))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def addLists[A,B,C](l1: List[A], l2: List[B])(f:(A,B) => C): List[C] = (l1, l2) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2), addLists(t1,t2)(f))
    }

  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => sub == Nil
    case _ if startsWith(l,sub) => true
    case Cons(_,t) => hasSubsequence(t, sub)
  }
}