package lab

import org.junit.Assert.*
import org.junit.Test
import List.*
import u02.Modules.Person

class ListPartOne:
  val lst = Cons (10 , Cons (20 , Cons (30 , Nil () )) )
  val lstFold = Cons (3 , Cons (7 , Cons (1 , Cons(5, Nil()) )) )
  @Test def testDrop() =
    assertEquals(drop(lst,1),Cons (20 , Cons(30, Nil())))
    assertEquals(drop(lst, 2), Cons(30, Nil()))
    assertEquals(drop(lst, 5), Nil())

  @Test def testAppend() =
    val tail = Cons(40, Nil())
    assertEquals(append(lst, tail),Cons (10 , Cons (20 , Cons (30 , Cons (40 , Nil ())))))

  @Test def testFlatMap() =
    var mom = flatMap(lst)(v => Cons ( v + 1, Nil()))
    assertEquals(mom, Cons (11 , Cons(21 , Cons(31 , Nil()))))
    mom = flatMap ( lst )(v => Cons ( v + 1, Cons (v + 2, Nil () )))
    val momT = Cons (11 , Cons (12 , Cons (21 , Cons (22 , Cons (31 , Cons (32 , Nil ()))))))
    assertEquals(mom, momT)

  @Test def testMax =
    assertEquals(max(Cons(10, Cons(25, Cons(20, Nil())))), Some(25))
    assertEquals(max(Nil()), None)

  @Test def testCourses() =
    val listPerson = Cons(Person.Teacher(" mario ", "fisica"), Cons(Person.Student(" mari ", 2015), Nil()))
    println(CoursesTeacher(listPerson))

  @Test def testFoldLeft() =
    assertEquals(foldLeft(lstFold)(0)(_-_), -16)

  @Test def testFoldRight() =
    assertEquals(foldRight(lstFold)(0)(_-_), -8)





