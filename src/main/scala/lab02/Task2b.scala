package lab02

object Task2b extends App:
  // 4)
  val p1: Int => Int => Int => Boolean = x => y => z => (y == z) && (x <= y)
  println("p1)")
  println(p1(2)(5)(5)) // true
  println(p1(5)(2)(3)) // false
  println(p1(2)(5)(4)) // false

  // Intermediate between p1 and p2
  // val px : Int => (Int, Int) => Boolean = x => (y, z) => (y == z) && (x <= y)

  val p2: (Int, Int, Int) => Boolean = (x, y, z) => (y == z) && (x <= y)
  println("\np2)")
  println(p2(2, 5, 5))  // true
  println(p2(5, 2, 3))  // false
  println(p2(2, 5, 4))  // false

  def p3(x: Int)(y: Int)(z: Int): Boolean = (y == z) && (x <= y)
  println("\np3)")
  println(p3(2)(5)(5)) // true
  println(p3(5)(2)(3)) // false
  println(p3(2)(5)(4)) // false

  def p4(x: Int, y: Int, z: Int): Boolean = (y == z) && (x <= y)
  println("\np4)")
  println(p4(2,5,5)) // true
  println(p4(5,2,3)) // false
  println(p4(2,5,4)) // false

  // 5)
  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))
  println("\n5)")
  println(compose(_ - 1, _ * 2)(5)) // 9

  def genericCompose[A, B, C, D <: A](f: A => B, g: C => D): C => B = (x: C) => f(g(x))

  def f1(s: String): Int = s.length
  def g1(x: String): String = x + " ciao"

  println(genericCompose(f1, g1)("ciao")) // 9

  def f2(b: Boolean): Int = b match
    case true => 2
    case false => -2

  def g2(s: String): Boolean = s.length > 5

  println(genericCompose(f2, g2)("ciao")) // -2
  println(genericCompose(f2, g2)("ciao lungo")) // 2

  def f3(x: Double): Int = x match
    case n if n >= 5.0 => 2
    case _ => -2

  def g3(s: String): Int = s.length

  println(genericCompose(f3, g3)("ciao")) // -2, OK because Int is subtype of Double
  println(genericCompose(f3, g3)("ciao lungo")) // 2

  def f4(x: Int): Int = x match
    case n if n >= 5.0 => 2
    case _ => -2

  def g4(s: String): Double = s.length
  //println(genericCompose(f4, g4)("ciao")) // ERROR because Double is not subtype of Int
