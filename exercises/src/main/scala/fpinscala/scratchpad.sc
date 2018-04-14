import fpinscala.datastructures.List
import fpinscala.datastructures.{Cons, List, Nil}

//val x = List(12,6,7,8) match {
//  case Cons(x, Cons(2, Cons(4, _))) => x
//  case Nil => 42
//  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
//  case Cons(h, t) => h + sum(t)
//  case _ => 101
//}

//reverse(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))))

List.appendViaFoldRight(List(1,2,3), List(4,5))



