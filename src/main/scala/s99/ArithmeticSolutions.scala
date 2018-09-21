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
    def goldbach: (Int, Int) = ???
  }

  def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m%n)

  def listPrimesinRange(r: Range): List[Int] = ???
  def printGoldbachList(r: Range): List[String] = ???
  def printGoldbachListLimited(r: Range, limit: Int): List[String] = ???

  // Optional but possibly useful exercise: not in original s-99 problems
  def primes: Stream[Int] = ???
}
