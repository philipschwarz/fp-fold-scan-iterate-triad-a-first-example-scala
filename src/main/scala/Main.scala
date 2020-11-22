import LazyList.iterate

def intToDigits(n: Int): Seq[Int] =
  iterate(n)(_ / 10)
    .takeWhile(_ != 0)
    .map(_ % 10)
    .toList
    .reverse  

extension (n:Int) 
  def ⊕ (d:Int) = 10 * n + d

def digitsToInt(ds: Seq[Int]): Int =
  ds.foldLeft(0)(_⊕_)
  
def digitsToSum(ds: Seq[Int]): Int =
  ds.foldLeft(0)(_+_)

def `convert⌛`(n: Int): Seq[(Int,Int)] =
  val segments = intToDigits(n).inits.toList.reverse
  val nums = segments map digitsToInt
  val sums = segments map digitsToSum
  nums zip sums

def `convert⚡`(n: Int): Seq[(Int,Int)] =
  val next: ((Int,Int),Int) => (Int,Int) =
    case ((number,sum),digit) => 
      (number ⊕ digit, sum + digit)
  intToDigits(n).scanLeft((0,0))(next)

@main def main =
  assert((123 ⊕ 4) == 1234)
  assert(intToDigits(12345) == List(1,2,3,4,5))
  assert(digitsToInt(List(1,2,3,4,5)) == 12345 )
  assert(digitsToSum(List(1,2,3,4,5)) == 15 )
  assert(`convert⌛`(12345) == List((0,0),(1,1),(12,3),(123,6),(1234,10),(12345,15)))
  assert(`convert⚡`(12345) == List((0,0),(1,1),(12,3),(123,6),(1234,10),(12345,15)))