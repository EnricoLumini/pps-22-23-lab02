package lab02

import lab02.Task4.Shape.Rectangle
import scala.math.*

object Task4 extends App:

  enum Shape:
    case Rectangle(l: Double, h: Double, center: (Double, Double))
    case Circle(r: Double, center:(Double, Double))
    case Square(l: Double, center: (Double, Double))

  object Shape:
    def perimeter(shape: Shape): Double = shape match
      case Rectangle(l, h, _) => 2*l + 2*h
      case Circle(r, _) => 2 * java.lang.Math.PI * r
      case Square(l, _) => 4 * l

    def contains(shape: Shape, point: (Double, Double)): Boolean = (shape, point)  match
      case (Rectangle(l, h, (ldcx, ldcy)), (x, y)) => (x >= ldcx && x <= l) && (y >= ldcy && y <= h)
      case (Circle(r, (cx, cy)), (x, y)) => pow((x - cx), 2) + pow((y - cy), 2) < pow(r, 2)
      case (Square(l, (ldcx, ldcy)), (x, y)) => (x >= ldcx && x <= l) && (y >= ldcy && y <= l)

  import Shape.*

  val rectangle: Rectangle = Rectangle(2, 3, (0, 0))
  val circle: Circle = Circle(3, (0, 0))
  val square: Square = Square(4, (0, 0))

  println(perimeter(rectangle)) // 10
  println(perimeter(circle)) // 18,849555922
  println(perimeter(square)) // 16

  println(contains(rectangle, (1, 1))) // true
  println(contains(rectangle, (3, 3))) // false
  println(contains(circle, (2, 2))) // true
  println(contains(circle, ( 4, 0))) // false
  println(contains(square, (2, 2))) // true
  println(contains(square, (5, 0))) // false