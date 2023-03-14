package lab02

object Task3 extends App:

  @annotation.tailrec
  def gcd(a: Int, b:Int): Int = (a, b) match
    case (n, m) if n > m && m != 0 => gcd(m, n % m)
    case (n, m) if m == 0 => n

  println(gcd(12, 8)) // 4
  println(gcd(14, 7)) // 7
