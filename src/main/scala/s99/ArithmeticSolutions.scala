package s99

trait ArithmeticSolutions {

  // add new functions to integers
  implicit class ExtendedInt(n: Int) {
    def isPrime: Boolean =
      if (n == 1) false
      else if (n == 2) true
      else !(2 until n).exists(x => n % x == 0)


    def isCoprimeTo(n: Int): Boolean = if(gcd(this.n, n) == 1) true else false
    def totient: Int = (for (r <- 1 to n if n.isCoprimeTo(r)) yield r).length
    def primeFactors: List[Int] = {
      def aux(n: Int, i: Int): List[Int]=
        if (n == 1) Nil
        else if (n % i == 0) i :: aux(n/i, i)
        else aux(n, n+1)

      aux(n,2)
    }

    def primeFactorMultiplicity: List[(Int, Int)] = new ListsSolutions {}.encodeDirect(this.primeFactors)

    def primeFactorMultiplicityMap: Map[Int, Int] = ???

    def improvedTotient: Int = n.primeFactorMultiplicity.map {
      case (p, m) => (p - 1) * math.pow(p, m - 1).toInt
    }.product

    def listPrimesinRange(r: Range): List[Int] = ???
    def goldbach: (Int, Int) = {
      val f: List[(Int, Int)] =
        for {
          x <- listPrimesinRange(1 until n)
          y <- listPrimesinRange(1 until n)
          if x+y == n
        } yield (x,y)

      f.head
    }

  }

  def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m%n)

  def listPrimesinRange(r: Range): List[Int] = (for ( i <- r if i.isPrime) yield i).toList
  def printGoldbachList(r: Range): List[String] =
    for {
      x <- listPrimesinRange(r)
      y <- listPrimesinRange(r.min until x)
      str <- x+y + " = $x + $y"
    } yield str.toString

  def printGoldbachListLimited(r: Range, limit: Int): List[String] =
    for {
      x <- listPrimesinRange(r)
      y <- listPrimesinRange(r.min until x)
      if x > 50 && y > 50
      str <- x+y + " = $x + $y"
    } yield str.toString

  // Optional but possibly useful exercise: not in original s-99 problems
  def primes: Stream[Int] = ???
}
