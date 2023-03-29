package lab

enum List[E]:
  case Cons(head: E, tail: List[E])
  case Nil()

object List:

  def drop[A](l: List[A], n: Int): List[A] = l match
    case Cons(head, tail) if n> 0 => drop(tail, n-1)
    case Cons(head, tail) if n==0 => Cons(head, tail)
    case Nil() => Nil()

  def append[A](left: List[A], right: List[A]): List[A] = left match
    case Cons(head, tail) => Cons(head, append(tail, right))
    case Nil() => right

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
    case Cons(head, tail) => append(f(head),flatMap(tail)(f))
    case Nil()=> Nil()

  def max(l: List[Int]): Option[Int] = l match
    case Cons(head, tail) if tail!=Nil() => if(head>max(tail).get) Some(head) else max(tail)
    case Cons(head, tail) if tail==Nil() => Some(head)
    case Nil()=> None




