package pfds

import scala.annotation.tailrec
import RedBlackSet._


case class RedBlackSet[A](tree: RedBlackSet.Tree[A])(implicit ordering :Ordering[A]) {

  @tailrec private def member(elem: A, tree: Tree[A]): Boolean = (elem, tree) match {
    case (x, Empty) => false
    case (x, Node(_, a, y, b)) =>
      if (ordering.lt(x, y)) member(x, a)
      else if (ordering.gt(x, y)) member(x, b)
      else true
  }

  private def balance(t: Node[A]) = t match {
    case Node(B, Node(R, Node(R, a, x, b), y, c), z, d) => Node(R, Node(B, a, x, b), y, Node(B, c, z, d))
    case Node(B, Node(R, a, x, Node(R, b, y, c)), z, d) => Node(R, Node(B, a, x, b), y, Node(B, c, z, d))
    case Node(B, a, x, Node(R, Node(R, b, y, c), z, d)) => Node(R, Node(B, a, x, b), y, Node(B, c, z, d))
    case Node(B, a, x, Node(R, b, y, Node(R, c, z, d))) => Node(R, Node(B, a, x, b), y, Node(B, c, z, d))
    case n => n
  }

  def contains(elem: A) = member(elem, tree)


  private def ins(x: A, t: Tree[A]): Node[A] = t match {
    case Empty => Node(R, Empty, x, Empty)
    case s@Node(color, a, y, b) =>
      if (ordering.lt(x, y)) balance(Node(color, ins(x, a), y, b))
      else if (ordering.gt(x, y)) balance(Node(color, a, y, ins(x, b)))
      else s
  }

  def +(elem: A) = copy(tree = ins(elem, tree).copy(color = B))




}


object RedBlackSet {

  sealed trait Color

  case object R extends Color

  case object B extends Color

  sealed trait Tree[+T]

  object Empty extends Tree[Nothing] {
    override def toString = "E"
  }

  case class Node[T](color: Color, l: Tree[T], elem: T, r: Tree[T]) extends Tree[T]

  def apply[T](elem:T*)(implicit ordering:Ordering[T]) :RedBlackSet[T] = elem.foldLeft(RedBlackSet[T](Empty)) { case (t, x) =>  t + x }
}
