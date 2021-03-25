package datastructure

import scala.annotation.tailrec


// singly linked list
sealed trait List[+A] {}
case object Nil extends List[Nothing]

case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {

    // def sum(list: List[Int]) : Int = list match {
    //     case Nil => 0 
    //     case Cons(h,t) => h + sum(t)
    // }

    def sum(list: List[Int]) : Int = {
        @annotation.tailrec
        def sum(list: List[Int], acc: Int) : Int = list match {
            case Nil => acc
            case Cons(h,t) => sum(t, acc + h)
        }
        sum(list, 0)
    }

    def sum(list: List[Int], acc: Int) : Int = list match {
        case Nil => acc
        case Cons(h,t) => sum(t, acc + h)
    }

    def product(list: List[Double]): Double = list match{
        case Nil => 1
        case Cons(h, t) => h * product(t) 
    }

    def apply[A](as: A*): List[A] = {
        if (as.isEmpty) Nil  // 대부분 isEmpty, null은 쓰지말자
        else Cons(as.head, apply(as.tail:_*)) // _* 가변인자로 변경해준다.
    }

    def setHead[A](list : List[A], head : A): List[A] = list match {
        case Cons(_, t) => Cons(head, t)
    }

    def tail[A](list : List[A]): List[A] = list match {
        case Cons(_, t) => t
    }

    @annotation.tailrec
    def drop[A](list : List[A], n: Int): List[A] = {
        if (n <= 0 ) list
        else drop(tail(list), n - 1)
    }

    // tailRec이 아님, memory leak 발생 가능, z : base, 
    def foldRight[A,B](list: List[A], z: B)(f: (A,B) => B): B = list match{
        case Nil => z
        case Cons(h,t) => f(h, foldRight(t, z)(f))
    }

    def sum2(list: List[Int]) =
        foldRight(list, 0)((x,y) => x + y)

    def product2(list: List[Double]) =
        foldRight(list, 1.0)((x,y) => x * y)

    @annotation.tailrec
    def foldLeft[A,B](list: List[A], z: B)(f: (B,A) => B): B = list match{
        case Nil => z
        case Cons(h,t) => foldLeft(t, f(z, h))(f)
    }

    def sumFoldLeft(list: List[Int]): Int = {
        foldLeft(list, 0)((x,y) => x + y)
    }

    def reverse[A](list: List[A]):List[A] =
        foldLeft(list, List(): List[A])((b,a) => Cons(a,b))

    def append[A](list: List[A], list2: List[A]):List[A] =
        foldLeft(reverse(list), list2)((b,a) => Cons(a,b))

    def concatenate[A](list: List[List[A]]): List[A] =
        foldLeft(reverse(list), List() :List[A])((b,a) => append(a,b))

    // memory leak 이 나지않는 foldRight.. 
    def foldRight2[A,B](list: List[A], z: B)(f: (A,B) => B): B =
        foldLeft(list, (b:B) => b)((g,a) => b => g(f(a, b)))(z)

    def addOne(list: List[Int]): List[Int] = 
        foldRight2(list, List(): List[Int])((a,b) => Cons(a + 1, b))

    def map[A,B](list: List[A])(f: A => B): List[B] = 
        foldRight2(list, List(): List[B])((h,t) => Cons(f(h), t))

    def filter[A](list: List[A])(f: A => Boolean): List[A] =
        foldRight2(list, List(): List[A])((a,b) => if (f(a)) Cons(a,b) else b)

    def flatMap[A,B](list: List[A])(f: A => List[B]): List[B] = 
        // concatenate(map(list)(f))
        foldRight2(list, List(): List[B])((a,b) => append(f(a), b))

    def flatMapToFilter[A](list: List[A])(f: A => Boolean): List[A] =
        flatMap(list)((a) => if (f(a)) List(a) else List())

    def zipWith[A, B, C](list: List[A], list2: List[B])(f: (A,B) => C): List[C] = (list, list2) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (_,_) => 
    }

    def main(args: Array[String]): Unit = {
        val list = List(1,2,3)

        // println(List(1,2,3))

        println(drop(list, 1))

        val list2 = List(list,List(4,5,6))

        // println(foldRight(list, 3))
        println(sumFoldLeft(list))

        println(concatenate(list2))

        println(addOne(list))

        println(map(list)(x => x+1))

        println(filter(list)(_ > 1))

        println(flatMapToFilter(list)(_ > 1))
    }
}
