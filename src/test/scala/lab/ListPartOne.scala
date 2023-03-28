package lab

import org.junit.Assert.*
import org.junit.Test
import List.*

class ListPartOne:
  val lst = Cons (10 , Cons (20 , Cons (30 , Nil () )) )
  @Test def testDrop() =
    assertEquals(drop(lst,1),Cons (20 , Cons(30, Nil())))
    assertEquals(drop(lst, 2), Cons(30, Nil()))
    assertEquals(drop(lst, 5), Nil())
