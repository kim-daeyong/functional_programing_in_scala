package candy

import scala.collection.immutable
import state.State
import state.State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = (i: Input) => (s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
  }

  // inputs 를 State로 바꾼다.
  // map foldRight unit modify get 
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    s <- get
    _ <- inputs.map( a -> )
  } yield ()

  def main(args: Array[String]): Unit = {
    val list = List(Machine(true, 2, 1))
    // println(simulateMachine())
  }
}
