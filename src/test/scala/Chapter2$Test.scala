import org.scalatest.FunSuite

class Chapter2$Test extends FunSuite {
  test("isSorted return true the array of Int is sorted"){
    //Given
    val arg = Array(1,2,3)
    val compareFunction = (first: Int, second: Int) => first < second

    //When
    val result = Chapter2.isSorted(arg, compareFunction)

    //Then
    assert(result)
  }

  test("isSorted return false the array of Int is sorted"){
    //Given
    val arg = Array(1,2,3,2)
    val compareFunction = (first: Int, second: Int) => first < second

    //When
    val result = Chapter2.isSorted(arg, compareFunction)

    //Then
    assert(!result)
  }

  test("isSorted return true when we sort strings by length"){
    //Given
    val arg = Array("a","aa","aaa","aaaa")
    val compareFunction = (first: String, second: String) => first.length < second.length

    //When
    val result = Chapter2.isSorted(arg, compareFunction)

    //Then
    assert(result)
  }

}
