package datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, t) => t
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => sys.error("setHead of empty list")
      case Cons(_, t) => Cons(h, t)
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, b) => b + 1)

  }

  def foldRight[A, B](l: List[A], b: B)(f: (A, B) => B): B = {
    l match {
      case Nil => b
      case Cons(h, t) => f(h, foldRight(t, b)(f))
    }
  }


  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    val g = (x: A, y: B) => f(y, x)
    foldRight(List.reverse(as), z)(g)
  }

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    val g = (x: B, y: A) => f(y, x)
    foldLeft(reverse(l), z)((x: B, y: A) => f(y, x))
  }

  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
  }

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def sum2(l: List[Int]): Int = {
    foldLeft(l, 0)(_ + _)
  }


  def product2(l: List[Int]): Int = {
    foldLeft(l, 1)(_ * _)
  }

  def length2[A](l: List[A]): Int = {
    foldLeft(l, 0)((acc: Int, a: A) => 1 + acc)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((acc: List[A], a: A) => Cons(a, acc))
  }


  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if (f(h)) => dropWhile(t, f)
      case _ => l
    }
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = {
    l1 match {
      case Nil => l2
      case Cons(h, t) => Cons(h, append(t, l2))
    }
  }

  def append_1[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)(Cons(_, _))
  }

  def concat[A](ll: List[List[A]]): List[A] = {
    foldLeft(ll, Nil: List[A])(append)
  }

  def add_1(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))
  }

  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil: List[B])((h, t) => List.append(f(h), t))
  }

  def flatMap2[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(x => if (f(x)) List(x) else Nil)
  }


  def addTwoLists(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addTwoLists(t1, t2))
    }
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }

  def hasSubsequent[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def hasSubsequentRec(p: List[A], b: List[A]): Boolean = {
      (p, b) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(hSup, tSup), Cons(hSub, tSub)) if hSup == hSub => hasSubsequentRec(tSup, tSub)
        case (Cons(hSup, tSup), Cons(hSub, tSub)) => hasSubsequentRec(tSup, sub)
      }
    }
    hasSubsequentRec(sup, sub)
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

//  val x = List(1, 2, 3, 4, 5) match {

//    case Cons(x, Cons(2, Cons(4, _))) => x
//    case Nil => 42
//    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
//    case Cons(h, t) => h + List.sum(t)
//    case _ => 101
//  }
//
