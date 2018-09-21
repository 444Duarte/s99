package s99

trait ListsSolutions {
  def last[T](list: List[T]): T = list match{
    case x :: Nil => x
    case _ :: xs => last(xs)
    case Nil => throw new NoSuchElementException
  }

  def penultimate[T](list: List[T]): T = list match {
    case x :: _ :: Nil => x
    case _ :: xs => penultimate(xs)
    case _ => throw new NoSuchElementException
  }

  def nth[T](n: Int, list: List[T]): T = 
    if (n == 0 ) list.head
    else nth (n-1, list.tail)

  def length[T](list: List[T]): Int = list match {
    case Nil => 0
    case x :: xs => length(xs) + 1
  }

  def reverse[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case x :: xs => reverse(xs) ::: List(x) 
  }

  def isPalindrome[T](list: List[T]): Boolean = list match {
    case Nil => true
    case x :: Nil => true
    case x :: xs => if (x == xs.last ) isPalindrome(list.tail.init) else false
  }

  def flatten(list: List[Any]): List[Any] = {
    def aux(elem: Any): List[Any] = elem match {
      case Nil => Nil
      case x :: xs => aux(x) ::: aux(xs)
      case x => List(x)
    }

    aux(list)
  }

  def compress[T](list: List[T]): List[T] = {
    def aux(curr: T, l: List[T]): List[T] = l match {
      case Nil => Nil
      case x :: Nil => if(x == curr) Nil else List(x)
      case x :: xs => if(x == curr) aux(curr, xs) else x :: aux(x,xs)
    }

    if(list.isEmpty) Nil
    else list.head :: aux(list.head, list.tail)
  }

  def pack[T](list: List[T]): List[List[T]] = {
    def aux (l: List[T], curr: List[T]): List[List[T]] = (l, curr) match {
      case (Nil,_) => List(curr)
      case (x :: xs, c :: cs) =>
        if(x == c) aux(xs, x :: curr )
        else curr :: aux(xs, List(x))
    }

    if(list.isEmpty) Nil
    else aux(list.tail, List(list.head))
  }

  def encode[T](list: List[T]): List[(Int, T)] = 
    pack(list) map(x => (x.length, x.head))

  def encodeModified[T](list: List[T]): List[Any] = 
    encode(list).map( x => if (x._1 == 1) x._2 else (x._1,x._2))

  def decode[T](list: List[(Int, T)]): List[T] = list.foldLeft[List[T]](Nil)( (b,x) => b ::: List.fill(x._1)(x._2))

  def encodeDirect[T](list: List[T]): List[(Int, T)] = {
    def aux(curr: (Int, T), l: List[T]): List[(Int, T)] = l match {
      case Nil => curr :: Nil
      case y :: Nil =>
        if(y == curr._2) (curr._1 + 1, y) :: Nil
        else List(curr, (1,y))
      case y :: ys =>
        if(y == curr._2) aux((curr._1 + 1, y), ys)
        else curr :: aux((1, y),ys)
    }

    list match {
      case Nil => Nil
      case x :: xs => aux((1,x), xs)
    }
  }

  def duplicate[T](list: List[T]): List[T] = ???

  def duplicateN[T](n: Int, list: List[T]): List[T] = ???
  def drop[T](n: Int, list: List[T]): List[T] = ???
  def split[T](n: Int, list: List[T]): (List[T], List[T]) = ???
  def slice[T](i: Int, j: Int, list: List[T]): List[T] = ???
  def rotate[T](n: Int, list: List[T]): List[T] = ???
  def removeAt[T](i: Int, list: List[T]): (List[T], T) = ???
  def insertAt[T](t: T, i: Int, list: List[T]): List[T] = ???
  def range[T](i: Int, j: Int): List[Int] = ???
  def randomSelect[T](n: Int, list: List[T]): List[T] = ???
  def lotto[T](i: Int, j: Int): List[Int] = ???
  def randomPermute[T](list: List[T]): List[T] = ???
  def combinations[T](n: Int, list: List[T]): List[List[T]] = ???
  def group3[T](list: List[T]): List[List[List[T]]] = ???
  def groups[T](ns: List[Int], list: List[T]): List[List[List[T]]] = ???
  def lsort[T](list: List[List[T]]): List[List[T]] = ???
  def lsortFreq[T](list: List[List[T]]): List[List[T]] = ???
}
