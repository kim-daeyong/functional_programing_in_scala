package example

import scala.annotation.tailrec  

object Hello extends Greeting with App {
  
  println(greeting)

  var a = 0
  for ( a <- 1 to 10) {
    println("test" + a)
  }

  var numList = List(1,2,3,4);

  numList = numList:+5
  numList = 0::numList
  numList = numList:::List(6)
  for ( a <- numList) {
    println(a)
  }

  println(numberMatch(3))

  def numberMatch(n : Int): String = n match {
  case 1 => "1"
  case 2 => "2"
  case (_) => "아님"
  }

  // @tailrec 
  def unsafeSum(n: Int): Int = {
    if (n == 1) 1  
    else n + unsafeSum(n - 1)
  }
  
  @tailrec
  def safeSum(n: Int, acc: Int): Int = {
    if (n == 0) acc  
    else safeSum(n - 1, n + acc)
  }

  println(safeSum(4, 0))
}

trait Greeting {
  lazy val greeting: String = "hello"
}

trait Fibo {

}

case class FiboTest(a : Int) extends Fibo{
  def test(a : Int) {

  }
}
