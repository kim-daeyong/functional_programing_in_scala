import candy.Candy._
import candy.Coin
import candy.Turn
import candy.Machine

object Main extends App {
  

  val inputCoin = List(Coin)
  val inputTurn = List(Turn)

  // Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
  val machine1 = Machine(true, 1, 0)
  println(simulateMachine(inputCoin).run(machine1))
  println(simulateMachine(inputTurn).run(machine1))
}