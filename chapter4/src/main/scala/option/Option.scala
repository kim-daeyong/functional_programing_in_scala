package option

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None: Option[B]
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this map f getOrElse None


  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(a => Some(a)).getOrElse(ob)

    /*
        value => f(value) => Option(value) - map/flatMap     
        구조를 변경하지않으며 값만 변경하는게 무슨의미인지
        구조를 변경하는 것은?
        왜 map을 썼는지 flatMap을 썼는지 알아야한다.      
    */
  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) this else None
  }

  def filter2(f: A => Boolean): Option[A] = 
    this flatMap (a => if (f(a)) this else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


// Container Type (List, Option, Tree) 
// (raw한)Int, Double .. -> Container(Int) 
// Int -> Int ===> Option(Int) -> Option(Int)

//Exceoption => base
// Exceoption or Value : Option
object Option {

    def add(n: Int): Int = n + 1
    
    def lift[A,B](f: A => B): Option[A] => Option[B] = _.map(f)

    def Try[A](a: => A):Option[A] =
        try Some(a)
        catch {
            case e: Exception => None // 핸들링 가능
        }
    def map2[A,B,C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] = (oa, ob) match {
        case (None, _) => None
        case (_, None) => None
        case (Some(a), Some(b)) => Some(f(a,b))
    }
    // how 보다 what을 생각하자 what 을 명확하면 how 가 나온다.
    def map3[A,B,C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] = 
        // oa  =>  ob  =>  oc  
        //   flatmap   map
        // oa.map(f(oa.flatMap(a => ), ob.flatMap(b => )))
        // oa.flatMap(f((oa, ob) => oa.map(), ob.map()))    // fail

        // oa.map(a => ob.map(b => f(a,b))).getOrElse(None)
        // =>
        oa.flatMap(a => ob.map(b => f(a,b)))

        //새로운 operation일까? map3로 map을 대체할수 있을까?

    /*
        a = findById()
        b = findById()
        a,b => 1촌관계 찾기
        List(a,b) : List[Option] => Option[List].map(ids => ~~~)
    */
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
        case Nil => None
        case h::t => map3(h, sequence(t))((hh, tt) => hh::tt)
    }

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
        case Nil => None
        // case h::t => traverse(t)(f).map(tt => f(h)::tt)
        case h::t => map3(f(h), traverse(t)(f))((hh,tt) => hh::tt)
    }

    // sequence와 traverse는 둘을 이용해서 만들 수 있을까? 둘 다 가능

    def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
        sequence(a.map(aa => f(aa)))

    def sequence2[A](a: List[Option[A]]): Option[List[A]] = ???
    
    

    def main(args: Array[String]): Unit = {
        // val a = new Exception("fail")
        val a = 3
        val oa = Try(a).map(_+1) // try catch 문은 사이드 이펙트이다. 이를 가려준다. exceoption을 숨기고 핸들링 할 수 있다.
        println(Some(4).map(_ => 5))
    }

}