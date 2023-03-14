package lab02

import lab02.Task4.Shape.*
import org.junit.Assert.*
import org.junit.*

class Task4Test:

  val rectangle: Rectangle = Rectangle(2, 3, (0, 0))
  val circle: Circle = Circle(3, (0, 0))
  val square: Square = Square(4, (0, 0))

  @Test def testPerimeter() =
    assertEquals(10, perimeter(rectangle), 0)
    assertEquals(18.85, perimeter(circle), 0.01)
    assertEquals(16, perimeter(square), 0)

  @Test def testContains() =
    assertTrue(contains(rectangle, (1, 1)))
    assertFalse(contains(rectangle, (3, 3)))
    assertTrue(contains(circle, (2, 2)))
    assertFalse(contains(circle, (4, 0)))
    assertTrue(contains(square, (2, 2)))
    assertFalse(contains(square, (5, 0)))


