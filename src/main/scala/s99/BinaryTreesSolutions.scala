package s99

trait BinaryTreesSolutions {

  sealed abstract class Tree[+T] {
    def isMirrorOf(that: Tree[_]): Boolean

    def isSymmetric: Boolean

    def addValue[S >: T](s: S)(implicit ev$1: S => Ordered[S]): Tree[S]

    def nodeCount: Int
    def leafCount: Int
    def leafList: List[T]
    def internalList: List[T]
    def atLevel(n: Int): List[T] = ???

    def layoutBinaryTree: Tree[T] = ???
    def layoutBinaryTree2: Tree[T] = ???
    def layoutBinaryTree3: Tree[T] = ???

    def show: String = ???
    def preOrder: List[T] = ???
    def inOrder: List[T] = ???
    def toDotString: String = ???

  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = s"T($value $left $right)"

    def isMirrorOf(that: Tree[_]): Boolean = that match {
      case End => false
      case Node(_, tLeft, tRight) =>
        left.isMirrorOf(tRight) && right.isMirrorOf(tLeft)
    }

    override def isSymmetric: Boolean = right.isMirrorOf(left)

    override def addValue[S >: T](s: S)(implicit ev$1: S => Ordered[S]): Tree[S] =
      if(s > value) Node(value, left, right.addValue(s))
      else Node(value, left.addValue(s), right)

    override def nodeCount: Int = 1 + left.nodeCount + right.nodeCount

    override def leafCount: Int =
      if(left == End && right == End) 1
      else left.leafCount + right.leafCount

    override def leafList: List[T] =
      if(left == End && right == End) List(value)
      else left.leafList ::: right.leafList

    override def internalList: List[T] =
      if(left == End && right == End) Nil
      else value :: left.internalList ::: right.internalList
  }

  case object End extends Tree[Nothing] {
    override def toString = "."

    override def isMirrorOf(that: Tree[_]): Boolean = that == End

    override def isSymmetric: Boolean = true

    override def addValue[S >: Nothing](s: S)(implicit ev$1: S => Ordered[S]): Tree[S] = Node(s,End, End)

    override def nodeCount: Int = 0

    override def leafCount: Int = 0

    override def leafList: List[Nothing] = Nil

    override def internalList: List[Nothing] = Nil
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  object Tree {
    def cBalanced[T](n: Int, e: T): List[Tree[T]] = {
      if (n == 0) List(End)
      else if ((n - 1) % 2 == 0) {
        val child = cBalanced((n - 1) / 2, e)
        for (left <- child; right <- child) yield Node(e, left, right)
      } else for {
        small <- cBalanced((n - 1) / 2, e)
        big <- cBalanced((n - 1) / 2 + 1, e)
        (left, right) <- List((small, big), (big, small))
      } yield Node(e, left, right)
    }

    def fromList[T: Ordering](list: List[T]): Tree[T] = ???
    def symmetricBalancedTrees[T](n: Int, e: T): List[Tree[T]] = ???
    def hbalTrees[T](h: Int, e: T): List[Tree[T]] = {
      if (h < 0) Nil
      else if (h == 0) List(End)
      else {
        var child = hbalTrees(h-1, e)
        val balanced = for (left <- child; right <- child) yield Node(e, left, right)

        val unbalanced = for {
          big <- child
          small <- hbalTrees(h-2, e)
          (left, right) <- List((small, big), (big, small))
        } yield Node(e, left, right)

        balanced ::: unbalanced
      }
    }

    def maxHbalNodes(h: Int): Int = math.pow(2, h).toInt -1

    def minHbalNodes(h: Int): Int = math.pow(2,h-1).toInt
    def maxHbalHeight(n: Int): Int = {
      def auxCycle(i: Int): Int = if(minHbalNodes(i) <= n && n <= maxHbalNodes(i)) i else auxCycle(i+1)

      auxCycle(0)
    }


    def hbalTreesWithNodes[T](n: Int, e: T): List[Tree[T]] = {
      val maxH = maxHbalHeight(n)
      val minH = (math.log(n) / math.log(2)).toInt

       hbalTrees(maxH, e).filter(_.nodeCount == n)
    }


    def completeBinaryTree[T](n: Int, e: T): Tree[T] = ???

    def fromString(string: String): Tree[Char] = ???
    def preInTree[T](pre: List[T], in: List[T]): Tree[T] = ???
    def fromDotString(string: String): Tree[Char] = ???
  }

  class PositionedNode[+T](override val value: T,
                           override val left: Tree[T],
                           override val right: Tree[T],
                           val x: Int,
                           val y: Int) extends Node[T](value, left, right) {
    override def toString = s"T[$x,$y]($value $left $right)"
  }

  object PositionedNode {
    def apply[T](value: T, left: Tree[T], right: Tree[T], x: Int, y: Int) =
      new PositionedNode[T](value, left, right, x, y)
    def unapply[T](p: PositionedNode[T]) =
      Some((p.value, p.left, p.right, p.x, p.y))
  }
}
