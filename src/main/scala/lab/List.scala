package lab

import u02.Modules.Person
import u02.Modules.Person.Teacher

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

  def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
  case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
  case Nil() => Nil()

  def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
  case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
  case Cons(_, t) => filter (t)(pred)
  case Nil() => Nil()


  def CoursesTeacher[A](l: List[A]): List[A] =
    var listT= filter(l)(_.isInstanceOf[Person.Teacher])
    return map(listT)(_.asInstanceOf[Person.Teacher].course.asInstanceOf[A])


  def foldLeft(l: List[Int])(defValue: Int)(f: (x:Int, y:Int) => Int): Int = l match
    case Cons(head, tail) if tail!=Nil() => f(foldLeft(tail)(defValue)(f), head)
    case Cons(head, tail) if tail==Nil() => f(defValue, head)
    case Nil() => defValue

  def foldRight(l: List[Int])(defValue: Int)(f: (Int, Int) => Int): Int = l match
    case Cons(head, tail) if tail != Nil()=> f(head, foldRight(tail)(defValue)(f))
    case Cons(head, tail) if tail == Nil() => f(head, defValue)
    case Nil() => defValue







