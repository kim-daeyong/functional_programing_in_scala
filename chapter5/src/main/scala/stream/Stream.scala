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
    def takeWhile(p: A => Boolean): Stream[A] = this match{
        case Cons(h, t) if p(h()) => cons(h(), t().takeWhile((p)))
        case _ => empty
    }

    // 한셋트씩
    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = (this, s2) match {
        case (Empty, _) => empty
        case (_, Empty) => empty
        case (Empty, Empty) => empty 
        case (Cons(h1, t1), Cons(h2, t2)) => cons(f(h1(), h2()), t1().zipWith(t2())(f))
    }

    // 다
    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = (this, s2) match {
        case (Empty, Empty) => empty 
        case (Empty, Cons(h, t)) => cons((None, Some(h())), this.zipAll(t()))
        case (Cons(h, t), Empty) => cons((Some(h()), None), t().zipAll(s2))
        case (Cons(h1, t1), Cons(h2, t2)) => cons((Some(h1()), Some(h2())), t1().zipAll(t2()))
    }

    //generate = unfold, why???
    def zipAllWithUnfold[B] (s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
        case (Cons(ah, at), Cons(bh, bt)) => Some((Some(ah()), Some(bh())), (at(), bt()))
    }

    def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = ???

    //스트림읠 열어보지않고 하나로 만든다.
    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Empty => z
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
    }

    // tolist = 스트림을 확인한다.
    // cons(1. ***)
    // unfold 스트림을 열지않은채 값을 확인한다.
    def startsWith[A](s: Stream[A]): Boolean = 
        unfold(this, s)({
            case (Empty, _) => None
            case (_, Empty) => None
            case (Cons(h1, t1), Cons(h2, t2)) => if(h1 ==  h2) Some((true, (t1(), t2()))) else Some((false, (t1(), t2())))
        }).forAll(_ == true)
    
    def mapViaFoldRight[B](f: A => B): Stream[B] =
        this.foldRight(empty: Stream[B])((a, b) => cons(f(a), b))

    def takeViaUnfold(n: Int): Stream[A] =
        unfold((this, n))({
            case (Cons(h, t), 1) => Some(h(), (empty, 0))
            case (Cons(h, t), n) => Some(h(), (t(), n-1))
            case (Empty, _) => None
        })

    def drop(n: Int): Stream[A] = this match {
        case Cons(h, t) if n > 0 => t().drop(n-1)
        case _ if n == 0 => this
        case _ => Empty
    }

    def exists(p: A => Boolean): Boolean = this match {
        case Empty => false
        case Cons(h, t) => if (p(h())) true else t().exists(p)
    }

    def exists2(p: A => Boolean): Boolean = 
        this.foldRight(false)((a, b) => p(a) | (b))

    def forAll(p: A => Boolean): Boolean =
        this.foldRight(true)((a, b) => p(a) & b)

    def headOption2: Option[A] =
        this.foldRight(None:Option[A])((a, b) => Some(a))
    
    def takeWhile2(p: A => Boolean): Stream[A] =
        this.foldRight(empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)
    
    def filter(f: A => Boolean): Stream[A] =
        this.foldRight(empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else b)

    def append[B >: A](target: => Stream[B]): Stream[B] =
        this.foldRight(target)(cons(_,_))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
        this.foldRight(empty: Stream[B])((a, b) => f(a).append(b))
    
    // this 1, 2, 3 / s 1, 2
    def startWith[A](s: Stream[A]): Boolean = 
        unfold((this, s))({
            case (_, Empty) => None
            case (Empty, _) => None
            case (Cons(h1, t1), Cons(h2, t2)) => if(h1 == h2) Some((true, (t1(), t2()))) else Some((false, (t1(), t2())))
        }).forAll(_ == true)

    def startWith2[A](s: Stream[A]): Boolean = 
        zipAll(s).takeWhile(o => !o._2.isEmpty).forAll({
            case (Some(h1), Some(h2)) => h1 == h2
        })

    // this 1, 2, 3 Stream(Steam(1,2,3), Stream(2,3), Stream(3), Stream())
    def tails: Stream[Stream[A]] = 
        unfold((this)){
            case Empty => None
            case s => Some(s, s drop 1)
        } append Stream(empty)
        

    // this 1, 2, 3 / s 2, 3
    def hasSubSequence[A](s: Stream[A]): Boolean =
        tails exists (_ startsWith(s))

    // this 1, 2, 3 base (a, b) => b : Stream[B]
    // unfold 는 왼쪽에서 오른쪽
    // foldRight 이용
    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = 
        foldRight((z, Stream(z)))((a, p0) => {
        // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
        lazy val p1 = p0 // foldright limit
        val b2 = f(a, p1._1)
        (b2, cons(b2, p1._2))
        })._2



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
    
    // 스트림 구축함수, 초기상태 하나와 다음상태 및 다음값을 산출하는 함수를 받는다
    // 스트림을 생성, 스트림을 펼칠 수 있는 함수? 
    def unfold[A, S](z: S)(f: S => Option[(A,S)]): Stream[A] = f(z) match {
            case None => empty
            case Some((h,s)) => cons(h, unfold(s)(f))
        }

    // def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    //     case empty => z
    //     case Cons(h, t) => f(h(), foldRight(t())(f))
    // }

    def ones:Stream[Int] = cons(1, ones)

    def ones2: Stream[Int] = unfold(1)(_ => Some(1, 1))


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

        // toList() 써야 (lazy)

        println(ones.mapViaFoldRight(_+1).take(5).toList)

        println()

    }


}