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
  // List[Input]  --- > State[Machine, (int, int)]
  // foldRight map unit modify get set
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =    for {
    s <- get
    _ <- sequence(inputs.map(i => modify(update(i))))

    // _ <- sequence(inputs map (modify[Machine] _ compose update))
  } yield (s.coins, s.candies)

  
  // for {
  //   s <- get
  //   _ <- sequence(inputs map { i => modify[Machine]((unit[Machine, (Int, Int)](i)))})

  //   // _ <- sequence(inputs map (modify[Machine] _ compose update))
  // } yield (s.coins, s.candies)


  // def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
  // for {
  //   _ <- sequence(inputs.map(i => modify((m: Machine) => (i, m) match {
  //     case (Coin, Machine(true, candy, coin)) if candy > 0 =>
  //       Machine(false, candy, coin + 1)
  //     case (Turn, Machine(false, candy, coin)) if candy > 0 =>
  //       Machine(true, candy - 1, coin)
  //     case _ => m
  //   })))
  //   m <- get
  // } yield (m.coins, m.candies)

  def main(args: Array[String]): Unit = {
    val ma = Machine(true, 2, 1)
  }
}
