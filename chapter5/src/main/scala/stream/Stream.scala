package stream

import stream.Stream._

/*
1. strict
2. state

1. strict

strict >: eager

어떻게 non strict를 실행 시킬것인가
내가 원할때 실행할 수 있다.
non-strict = laziness

map filter 로 8번에 걸쳐 할 것을 for 문으로 4번정도로 할 수 있다.
비효율적인가?

이걸 해결하기위해 laziness

=> List().map().filter().findFirst().take..
=> stream이 laziness를 지원한다. 

2. state
    immutable pure function
    어떠한 function, object => state => cache, memory.. (data 가지고 있는 애들)
    pure fuction은 state를 가질수 없으나 어떻게 가지게 할 것 인가.


non-strict 3가지 문법 (Evalueate 되지않은 상태로 가지고 있음)

onTrue:  => A, onFalse: => A  //기존 () => 

lazy val  


* 자바는 Supplier를 이용한다.
* for loop 이 Stream 보다 빠르다? => lazy하게 사용하면 됨
*/


// strict,  non-strict 개념을 이해해야 한다.    , 마지막 에 하나씩 오퍼레이션이 실행된다.
sealed trait Stream[+A] {

    def take(n: Int): Stream[A] = this match {
        case Cons(h, t) if n > 0 => cons(h(), t().take(n-1))
        // case _ => Nil
    }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }


}