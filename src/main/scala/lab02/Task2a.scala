package lab02

object Task2a extends App:

  // a)
  val valPositive: Int => String = _ match
    case n if n >= 0 => "positive"
    case _ => "negative"

  def methodPositive(n: Int): String = n match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println("a)")
  println(valPositive(10))
  println(valPositive(-10))

  println(methodPositive(10))
  println(methodPositive(-10))

  // b)
  val valNeg: (String => Boolean) => (String => Boolean) = f => (s => !f(s))
  def methodNeg(f: String => Boolean): String => Boolean = s => !f(s)

  val empty: String => Boolean = _ == ""
  val valNotEmpty: String => Boolean = valNeg(empty)
  val methodNotEmpty: String => Boolean = methodNeg(empty)
  println("b)")
  println(valNotEmpty("foo"))
  println(valNotEmpty(""))

  println(methodNotEmpty("foo"))
  println(methodNotEmpty(""))

  // c)
  def genericNeg[A](f: A => Boolean): A => Boolean = s => !f(s)

  def positive(n: Int): Boolean = n match
    case n if n > 0 => true
    case _ => false

  println("c)")
  println(positive(2))
  println(positive(-2))
  val negative: Int => Boolean = genericNeg(positive)
  println(negative(2))
  println(negative(-2))

  def isStringEmpty(s: String): Boolean = s.isEmpty()

  println(isStringEmpty(""))
  println(isStringEmpty("ciao"))
  val isStringNotEmpty: String => Boolean = genericNeg(isStringEmpty)
  println(isStringNotEmpty(""))
  println(isStringNotEmpty("ciao"))