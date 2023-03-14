package lab02

import scala.math.pow

object AllTasks extends App:
  // Task 1
  println("Hello, Scala")

  // Task 2a
  // a)
  println("\n\nTask 2a tests")
  val valPositive: Int => String = _ match
    case n if n >= 0 => "positive"
    case _ => "negative"

  def methodPositive(n: Int): String = n match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println(">Test positive (a)")
  println("Expected: " + "positive" + ", Actual: " + valPositive(10))
  println("Expected: " + "negative" + ", Actual: " + valPositive(-10))

  println("Expected: " + "positive" + ", Actual: " + methodPositive(10))
  println("Expected: " + "negative" + ", Actual: " + methodPositive(-10))

  // b)
  val valNeg: (String => Boolean) => String => Boolean = f => s => !f(s)

  def methodNeg(f: String => Boolean): String => Boolean = s => !f(s)

  val empty: String => Boolean = _ == ""
  val valNotEmpty: String => Boolean = valNeg(empty)
  val methodNotEmpty: String => Boolean = methodNeg(empty)
  println("\n>Test neg (b)")
  println("Expected: " + true + ", Actual: " + valNotEmpty("foo"))
  println("Expected: " + false + ", Actual: " + valNotEmpty(""))

  println("Expected: " + true + ", Actual: " + methodNotEmpty("foo"))
  println("Expected: " + false+ ", Actual: " + methodNotEmpty(""))

  // c)
  def genericNeg[A](f: A => Boolean): A => Boolean = s => !f(s)

  def positive(n: Int): Boolean = n match
    case n if n > 0 => true
    case _ => false

  println("\n>Test generic neg (c)")
  println("Expected: " + true + ", Actual: " + positive(2))
  println("Expected: " + false + ", Actual: " + positive(-2))
  val negative: Int => Boolean = genericNeg(positive)
  println("Expected: " + false + ", Actual: " + negative(2))
  println("Expected: " + true + ", Actual: " + negative(-2))

  def isStringEmpty(s: String): Boolean = s.isEmpty

  println("Expected: " + true + ", Actual: " + isStringEmpty(""))
  println("Expected: " + false + ", Actual: " + isStringEmpty("ciao"))
  val isStringNotEmpty: String => Boolean = genericNeg(isStringEmpty)
  println("Expected: " + false + ", Actual: " + isStringNotEmpty(""))
  println("Expected: " + true + ", Actual: " + isStringNotEmpty("ciao"))


  // Task 2b
  // 4)
  println("\n\nTask 2b tests")
  val p1: Int => Int => Int => Boolean = x => y => z => (y == z) && (x <= y)
  println(">Curried val test")
  println("Expected: " + true + ", Actual: " + p1(2)(5)(5))
  println("Expected: " + false + ", Actual: " + p1(5)(2)(3))
  println("Expected: " + false + ", Actual: " + p1(2)(5)(4))

  val p2: (Int, Int, Int) => Boolean = (x, y, z) => (y == z) && (x <= y)
  println("\n>Not curried val test")
  println("Expected: " + true + ", Actual: " + p2(2, 5, 5))
  println("Expected: " + false + ", Actual: " + p2(5, 2, 3))
  println("Expected: " + false + ", Actual: " + p2(2, 5, 4))

  def p3(x: Int)(y: Int)(z: Int): Boolean = (y == z) && (x <= y)

  println("\n>Curried method test")
  println("Expected: " + true + ", Actual: " + p3(2)(5)(5))
  println("Expected: " + false + ", Actual: " + p3(5)(2)(3))
  println("Expected: " + false + ", Actual: " + p3(2)(5)(4))

  def p4(x: Int, y: Int, z: Int): Boolean = (y == z) && (x <= y)

  println("\n>Not curried method test")
  println("Expected: " + true + ", Actual: " + p4(2, 5, 5))
  println("Expected: " + false + ", Actual: " + p4(5, 2, 3))
  println("Expected: " + false + ", Actual: " + p4(2, 5, 4))

  // 5)
  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))

  println("\n>Compose test")
  println("Expected: " + 9 + ", Actual: " + compose(_ - 1, _ * 2)(5))

  def genericCompose[A, B, C, D <: A](f: A => B, g: C => D): C => B = (x: C) => f(g(x))

  def f1(s: String): Int = s.length
  def g1(s: String): String = s + " ciao"

  println("\n>Generic compose test")
  println("Expected: " + 9 + ", Actual: " + genericCompose(f1, g1)("ciao"))

  def f2(b: Boolean): Int = b match
    case true => 2
    case false => -2

  def g2(s: String): Boolean = s.length > 5

  println("Expected: " + -2 + ", Actual: " + genericCompose(f2, g2)("ciao"))
  println("Expected: " + 2 + ", Actual: " + genericCompose(f2, g2)("ciao lungo"))

  def f3(x: Double): Int = x match
    case n if n >= 5.0 => 2
    case _ => -2

  def g3(s: String): Int = s.length

  println("Expected: " + -2 + ", Actual: " + genericCompose(f3, g3)("ciao")) // OK because Int is subtype of Double
  println("Expected: " + 2 + ", Actual: " + genericCompose(f3, g3)("ciao lungo"))

  def f4(x: Int): Int = x match
    case n if n >= 5.0 => 2
    case _ => -2

  def g4(s: String): Double = s.length
//println(genericCompose(f4, g4)("ciao")) // ERROR because Double is not subtype of Int


  // Task 3
  println("\n\nTask 3 test")
  @annotation.tailrec
  def gcd(a: Int, b: Int): Int = (a, b) match
    case (n, m) if n > m && m != 0 => gcd(m, n % m)
    case (n, m) if m == 0 => n

  println(">GCD test:")
  println("Expected: " + 4 + ", Actual: " + gcd(12, 8))
  println("Expected: " + 7 + ", Actual: " + gcd(14, 7))


  // Task 4
  println("\n\nTask 4 tests")
  enum Shape:
    case Rectangle(l: Double, h: Double, corner: (Double, Double))  // buttom left corner
    case Circle(r: Double, center: (Double, Double))  // center of circle
    case Square(l: Double, corner: (Double, Double))  // buttom left corner

  object Shape:

    def perimeter(shape: Shape): Double = shape match
      case Rectangle(l, h, _) => 2 * l + 2 * h
      case Circle(r, _) => 2 * java.lang.Math.PI * r
      case Square(l, _) => 4 * l

    def contains(shape: Shape, point: (Double, Double)): Boolean = (shape, point) match
      case (Rectangle(l, h, (blcx, blcy)), (x, y)) => (x >= blcx && x <= l) && (y >= blcy && y <= h)
      case (Circle(r, (cx, cy)), (x, y)) => pow(x - cx, 2) + pow(y - cy, 2) < pow(r, 2)
      case (Square(l, (blcx, blcy)), (x, y)) => (x >= blcx && x <= l) && (y >= blcy && y <= l)

  import Shape.*

  val rectangle: Rectangle = Rectangle(2, 3, (0, 0))
  val circle: Circle = Circle(3, (0, 0))
  val square: Square = Square(4, (0, 0))

  println(">Perimeter test")
  println("Expected: " + 10.0 + ", Actual: " + perimeter(rectangle))
  println("Expected: " + 18.85 + " (delta: " + 0.1 + "), Actual: " + perimeter(circle))
  println("Expected: " + 16.0 + ", Actual: " + perimeter(square))

  println("\n>Contains test")
  println("Expected: " + true + ", Actual: " + contains(rectangle, (1, 1)))
  println("Expected: " + false + ", Actual: " + contains(rectangle, (3, 3)))
  println("Expected: " + true + ", Actual: " + contains(circle, (2, 2)))
  println("Expected: " + false + ", Actual: " + contains(circle, (4, 0)))
  println("Expected: " + true + ", Actual: " + contains(square, (2, 2)))
  println("Expected: " + false + ", Actual: " + contains(square, (5, 0)))


  // Task 5
  println("\n\nTask 5 tests")

  enum Option[A]:
    case Some(a: A)
    case None()

  object Option:

    def isEmpty[A](opt: Option[A]): Boolean = opt match
      case None() => true
      case _ => false

    def orElse[A, B >: A](opt: Option[A], orElse: B): B = opt match
      case Some(a) => a
      case _ => orElse

    def flatMap[A, B](opt: Option[A])(f: A => Option[B]): Option[B] = opt match
      case Some(a) => f(a)
      case _ => None()

    def filter[A](opt: Option[A])(f: A => Boolean): Option[A] = (opt, f) match
      case (Some(a), f) if f(a) => Some(a)
      case _ => None()

    def map[A](opt: Option[A])(f: A => Boolean): Option[Boolean] = (opt, f) match
      case (Some(a), f) => Some(f(a))
      case _ => None()

    def fold[A](opt: Option[A])(df: A)(f: A => A): A = (opt, df, f) match
      case (Some(a), _, f) => f(a)
      case (_, d, _) => d

  import Option.*

  println(">Filter test")
  println("Expected: " + Some(5) + ", Actual: " + filter(Some(5))(_ > 2))
  println("Expected: " + None() + ", Actual: " + filter(Some(5))(_ > 8))
  println("Expected: " + None() + ", Actual: " + filter(None[Int]())(_ > 2))
  println("Expected: " + Some("ciao") + ", Actual: " + filter(Some("ciao"))(_.length <= 5))
  println("Expected: " + None() + ", Actual: " + filter(Some("ciao"))(_.isEmpty))

  println("\n>Map test")
  println("Expected: " + Some(true) + ", Actual: " + map(Some(5))(_ > 2))
  println("Expected: " + Some(false) + ", Actual: " + map(Some(5))(_ > 8))
  println("Expected: " + None() + ", Actual: " + map(None[Int]())(_ > 2))

  println("\n>Fold test")
  println("Expected: " + 6 + ", Actual: " + fold(Some(5))(1)(_ + 1))
  println("Expected: " + 1 + ", Actual: " + fold(None[Int]())(1)(_ + 1))
  println("Expected: " + "Ciao mondo!" + ", Actual: " + fold(Some("Ciao"))("a caso")(_ concat " mondo!"))
  println("Expected: " + "default" + ", Actual: " + fold(None[String]())("default")(_ concat " mondo!"))