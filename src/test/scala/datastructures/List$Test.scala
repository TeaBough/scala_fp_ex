package datastructures

import org.scalatest.FunSuite

class List$Test extends FunSuite {

  test("testDrop") {
    //Given
    val l = List(1, 2, 3, 4)

    //When
    val dl = List.drop(l, 2)

    //Then
    assert(dl === Cons(3, Cons(4, Nil)))

  }

  test("testDropWhile") {
    //Given
    val l = List(2, 4, 6, 7)

    //When
    val dl = List.dropWhile(l, (a: Int) => a % 2 == 0)

    //Then
    assert(dl === Cons(7, Nil))
  }

  test("testFoldRight") {
    //Given
    val l = List(1, 2, 3)

    //When
    val result = List.foldRight(l, 0)(_ + _)

    //Then
    assert(result === 6)
  }

  test("testSum2") {
    //Given
    val l = List(1, 2, 3)
    //When
    val result = List.sum(l)

    //Then
    assert(result === 6)
  }

  test("testFoldLeft") {
    //Given
    val l = List(1, 2, 3)

    //When
    val result = List.foldLeft(l, 0)(_ + _)

    //Then
    assert(result === 6)
  }

  test("testFoldLeft2") {
    //Given
    val l = List(1, 2, 3)

    //When
    val result = List.foldLeft2(l, 0)(_ + _)

    //Then
    assert(result === 6)
  }

  test("testFoldRightViaFoldLeft") {
    //Given
    val l = List(1, 2, 3)

    //When
    val result = List.foldRightViaFoldLeft(l, 0)(_ + _)

    //Then
    assert(result === 6)
  }

  test("testAppend") {
    //Given
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)

    //When
    val ll = List.append(l1, l2)
    //Then
    assert(ll === Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil)))))))
  }

  test("testAppend_1") {
    //Given
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)

    //When
    val ll = List.append_1(l1, l2)
    //Then
    assert(ll === Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil)))))))
  }

  test("testAdd1") {
    //Given
    val l = List(1, 2, 3, 4)

    //When
    val lPlusOne = List.add_1(l)

    //Then
    assert(lPlusOne === Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
  }

  test("testDoubleToString") {
    //Given
    val l = List(1.0, 2.2, 3.2, 4.3)

    //When
    val lString = List.doubleToString(l)

    //Then
    assert(lString === Cons("1.0", Cons("2.2", Cons("3.2", Cons("4.3", Nil)))))
  }

  test("testMap") {
    //Given
    val l = List(1.0, 2.2, 3.2, 4.3)

    //When
    val lString = List.map(l)(_.toString)

    //Then
    assert(lString === Cons("1.0", Cons("2.2", Cons("3.2", Cons("4.3", Nil)))))
  }

  test("testFilter") {
    //Given
    val l = List(1, 2, 3, 4)

    //When
    val lFilter = List.filter(l)(_ % 2 == 0)

    //Then
    assert(lFilter === Cons(2, Cons(4, Nil)))

  }

  test("testFilterWithFlatMap") {
    //Given
    val l = List(1, 2, 3, 4)

    //When
    val lFilter = List.filterWithFlatMap(l)(_ % 2 == 0)

    //Then
    assert(lFilter === Cons(2, Cons(4, Nil)))

  }

  test("testAddTwoLists") {
    //Given
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)

    //When
    val ll = List.addTwoLists(l1, l2)

    //Then
    assert(ll === Cons(5, Cons(7, Cons(9, Nil))))
  }

  test("testZipWith") {
    //Given
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)

    //When
    val ll = List.zipWith(l1, l2)(_+_)

    //Then
    assert(ll === Cons(5, Cons(7, Cons(9, Nil))))

  }

  test("hasSubsequent when hasSubsequent") {
    //Given
    val l1 = List(1, 2, 3, 4)
    val l2 = List(3, 4)

    //When
    val isSub = List.hasSubsequent(l1,l2)

    //Then
    assert(isSub)
  }

  test("hasSubsequent when has no subsequent") {
    //Given
    val l1 = List(1, 2, 3, 4)
    val l2 = List(5, 4)

    //When
    val isSub = List.hasSubsequent(l1,l2)

    //Then
    assert(!isSub)
  }

  test("flatMap") {
    //Given
    val l = List(1, 2, 3, 4)

    //When
    val ll = List.flatMap(l)(i => List(i, i))

    //Then
    assert(ll === Cons(1, Cons(1, Cons(2, Cons(2, Cons(3, Cons(3, Cons(4, Cons(4, Nil)))))))))

  }


  test("testConcat") {
    //Given
    val ll = List(List(1, 2), List(3, 4), List(5, 6))

    //When
    val flatList = List.concat(ll)

    //Then
    assert(flatList === Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil)))))))
  }


  test("testLength2") {
    //Given
    val l = List(1, 2, 3, 4, 5)
    //When

    val size = List.length2(l)

    //Then
    assert(size === 5)
  }

  test("testReverse") {
    //Given
    val l = List(1, 2, 3, 4)

    //When
    val ll = List.reverse(l)
    //Then
    assert(ll === Cons(4, Cons(3, Cons(2, Cons(1, Nil)))))
  }

  test("testLength") {
    //Given
    val l = List(1, 2, 3, 4, 5)
    //When

    val size = List.length(l)

    //Then
    assert(size === 5)
  }

  test("testInit") {
    //Given
    val l = List(1, 2, 3, 4)

    //When
    val dl = List.init(l)

    //Then
    assert(dl === Cons(1, Cons(2, Cons(3, Nil))))
  }

  test("testSetHead") {
    //Given
    val l = List(1, 2, 3, 4)

    //When
    val t = List.setHead(l, 5)

    //Then
    assert(t === Cons(5, Cons(2, Cons(3, Cons(4, Nil)))))
  }

  test("testTail") {
    //Given
    val l = List(1, 2, 3, 4)

    //When
    val t = List.tail(l)

    //Then
    assert(t === Cons(2, Cons(3, Cons(4, Nil))))
  }

}
