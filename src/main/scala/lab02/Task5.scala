package lab02

object Task5 extends App:

  enum Option[A]:
    case Some(a: A)
    case None() // here parens are needed because of genericity

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

    def fold[A](opt: Option[A])(default: A)(f: A => A): A = (opt, default, f) match
      case (Some(a), _, f) => f(a)
      case (_, d, _) => d

  import Option.*

  println("Filter tests:")
  println(filter(Some(5))(_ > 2)) // Some(5)
  println(filter(Some(5))(_ > 8)) // None
  println(filter(None[Int]())(_ > 2)) // None
  println(filter(Some("ciao"))(_.length <= 5)) // Some(ciao)
  println(filter(Some("ciao"))(_.isEmpty)) // None
  println("\nMap tests:")
  println(map(Some(5))(_ > 2)) // Some(true)
  println(map(Some(5))(_ > 8)) // Some(false)
  println(map(None[Int]())(_ > 2)) // None
  println("\nFold tests:")
  println(fold(Some(5))(1)(_ + 1)) // 6
  println(fold(None[Int]())(1)(_ + 1)) // 1
  println(fold(Some("Ciao"))("a caso")(_ concat " mondo!")) // Ciao mondo!
  println(fold(None[String]())("default")(_ concat "Ciao mondo!")) // default