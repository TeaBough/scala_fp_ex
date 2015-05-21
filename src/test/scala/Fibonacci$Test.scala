import org.scalatest.FunSuite

class Fibonacci$Test extends FunSuite {
  test("Fibo 0 is 0") {
    //Given
    val arg = 0

    //When
    val result = Fibonacci.compute(arg)

    //Then
    assert(result === 0)
  }

  test("Fibo 1 is 1") {
    //Given
    val arg = 1

    //When
    val result = Fibonacci.compute(arg)

    //Then
    assert(result === 1)
  }

  test("Fibo 2 is 1") {
    //Given
    val arg = 2

    //When
    val result = Fibonacci.compute(arg)

    //Then
    assert(result === 1)
  }

  test("Fibo 3 is 2") {
    //Given
    val arg = 3

    //When
    val result = Fibonacci.compute(arg)

    //Then
    assert(result === 2)
  }

  test("Fibo 4 is 3") {
    //Given
    val arg = 4

    //When
    val result = Fibonacci.compute(arg)

    //Then
    assert(result === 3)
  }

  test("Fibo 20 is 6765") {
    //Given
    val arg = 20

    //When
    val result = Fibonacci.compute(arg)

    //Then
    assert(result === 6765)
  }
}
