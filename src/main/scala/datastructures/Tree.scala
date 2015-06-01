package datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size(t: Tree[Int]): Int = {
    t match {
      case Branch(l, r) => 1 + size(l) + size(r)
      case Leaf(_) => 1
    }
  }

  def max(t: Tree[Int]): Int = {
    t match {
      case Branch(l, r) => max(l) max max(r)
      case Leaf(l) => l
    }
  }

  def depth(t: Tree[Int]): Int = {
    t match {
      case Branch(l, r) => 1 + depth(l) max depth(r)
      case Leaf(_) => 1
    }
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      case Leaf(l) => Leaf(f(l))
    }
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
      case Leaf(l) => f(l)
    }
  }

  def size_2(t: Tree[Int]): Int = {
    fold(t)((x: Int) => 1)((x, y) => 1 + x + y)
  }

  def max_2(t: Tree[Int]): Int = {
    fold(t)((x: Int) => x)((x, y) => x max y)
  }

  def depth_2(t: Tree[Int]): Int = {
    fold(t)((x: Int) => 1)((x, y) => 1 + x max y)
  }

  def map_2[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)((x: A) => Leaf(f(x)): Tree[B])(Branch(_, _))
  }
}
