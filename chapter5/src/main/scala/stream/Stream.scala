package stream

import stream.Stream._

/*
1. strict
2. state

1. strict

    엄격한 함수 (strict function) : 인수를 항상 Evaluation 한다. 
    strict >: eager

어떻게 non strict를 실행 시킬것인가
이는 함수를 내가 원할때 실행할 수 있다.
non-strict >: laziness


//
map filter 로 8번에 걸쳐 할 것을 for 문으로 4번정도로 할 수 있다.
비효율적인가?

이걸 해결하기위해 laziness

=> List().map().filter().findFirst().take..
=> stream이 laziness를 지원한다. 

* 자바는 Supplier를 이용한다.
* for loop 이 Stream 보다 빠르다? => lazy하게 사용하면 됨
//

2. state
    immutable pure function
    어떠한 function, object => state => cache, memory.. (data 가지고 있는 애들)
    pure fuction은 state를 가질수 없으나 어떻게 가지게 할 것 인가.


non-strict 3가지 문법 (Evalueate 되지않은 상태로 가지고 있음)

onTrue:  => A, onFalse: => A  //기존 () => 

lazy val  

*/

// strict,  non-strict 개념을 이해해야 한다, 마지막에 하나씩 오퍼레이션이 실행된다.
sealed trait Stream[+A] {

    def headOption: Option[A] = this match {
        case Cons(h, t) => Some(h())
        case Empty => None
    }

    def toList: List[A] = this match {
        case Cons(h, t) => h()::t().toList
        case Empty => Nil
    }

    // n개만 받는다.
    def take(n: Int): Stream[A] = this match {
        case Cons(h, t) if n > 0 => cons(h(), t().take(n-1))
        case _ => empty
    }

    // def map[B](f:A => B): Stream[B] = this match {
    //     case Cons(h, t) => cons(f(h()), t().map((f)))
    //     case Empty => empty
    // }

    /*
        Stream(1,2,3).map(_+1).toList
        Con(1,Stream(2,3))
        Some((f(1), Stream(2,3))
        Some((f(2), Stream(3))
        Some((f(3), Empty))
        None

        => List(1,2,3)
    */

    def map[B](f:A => B): Stream[B] = 
        unfold(this) {
            case Cons(h, t) => Some((f(h()), t()))
            case Empty => None
        }

    // boolean 통과만
    def takeWhile(p: A => Boolean): Stream[A] = ???

    // 한셋트씩
    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = ???

    // 다
    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = ???

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    
    def unfold[A, S](z: S)(f: S => Option[(A,S)]): Stream[A] = ???

    def main(args: Array[String]): Unit = {

        // a.flatMap(aa => b.map(bb => aa+bb)
        // map2
        // for {
        //     aa <-list1
        //     bb <-list2
        // } yeild {
        //     aa+bb
        // }
        // map(List(1,2,3), List(4,5,6)(_+_))

    }


}