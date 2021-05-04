sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = ???
    // (i: Input) => (s: Machine) =>
    // (i, s) match {
    // }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
  
}
