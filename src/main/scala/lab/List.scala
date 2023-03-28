package lab

enum List[E]:
  case Cons(head: E, tail: List[E])
  case Nil()

object List:

  def drop[A](l: List[A], n: Int): List[A] = l match
    case Cons(head, tail) if n> 0 => drop(tail, n-1)
    case Cons(head, tail) if n==0 => Cons(head, tail)
    case Nil() => Nil()
