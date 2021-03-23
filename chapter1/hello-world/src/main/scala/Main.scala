
object Main extends App {
  println("Hello, World!")
}

// side effect 가 없는 함수 : pure function

class Cafe {
  // def buyCoffee(cc: CreditCard): Coffee = {
  //   val cup = new Coffee()
  //   cc.charge(cup.price)
  //   cup
  // }

  // def buyCoffee(cc: CreditCard, p: Payments): Coffee = {
  //   val cup = new Coffee()
  //   p.charge(cc, cup.price)
  //   cup
  // }

  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee()
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n : Int): (List[Coffee], Charge) = {
    val cup = new Coffee()
    val purchases : List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1,c2) => c1.combine(c2)))
  }

  def coalesce(charges: List[Charge]) : List[Charge] =
  charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
}

class Coffee {
  val price = 5000;
}

class CreditCard {
  def charge(price : Int) {

  }
}

case class Charge(cc : CreditCard, amount : Double) {
  def combine(other: Charge): Charge = 
    if (cc == other.cc) Charge(cc, amount + other.amount)
    else throw new Exception("cant combie")
}

trait Payments {
  def charge(cc : CreditCard, price : Int) = {

  }
}