object Fibonacci {
  def compute(n: Int): Int = {
    @annotation.tailrec
    def fibo(cursor: Int, firstNumber: Int, secondNumber: Int): Int = {
      if (cursor == n) firstNumber else fibo(cursor + 1, secondNumber + firstNumber, firstNumber)
    }
    fibo(0, 0, 1)
  }
}


