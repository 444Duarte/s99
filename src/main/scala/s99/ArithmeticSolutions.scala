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
        else if (n % i == 0) i :: aux(n / i, i)
        else aux(n, i + 1)

      aux(n,2)
    }

    def primeFactorMultiplicity: List[(Int, Int)] = new ListsSolutions {}.encode(primeFactors).map(_.swap)
    def primeFactorMultiplicityMap: Map[Int, Int] = ???

    def improvedTotient: Int = primeFactorMultiplicity.map {
      case (p, m) => (p - 1) * math.pow(p, m - 1).toInt
    }.product

    def goldbach: (Int, Int) = {
      val f: List[(Int, Int)] =
        for {
          x <- listPrimesinRange(1 until n)
          y = n-x
          if x+y == n && y.isPrime
        } yield (x,y)

      f.head
    }

  }

  def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m%n)

  def listPrimesinRange(r: Range): List[Int] = r.filter(_.isPrime).toList
  def printGoldbachList(r: Range): List[String] = printGoldbachListLimited(r,0)

  def printGoldbachListLimited(r: Range, limit: Int): List[String] = r.foldRight[List[String]](Nil){ (n, acc) =>
    if(n <= 2 || n % 2 != 0) acc
    else {
      val (x, y) = n.goldbach
      if(x > limit) n + s" = $x + $y" :: acc
      else acc
    }
  }





  // Optional but possibly useful exercise: not in original s-99 problems
  def primes: Stream[Int] = ???
}
