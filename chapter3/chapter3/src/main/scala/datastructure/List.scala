package datastructure

import scala.annotation.tailrec

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

    // tailRec이 아님, memory lick 발생 가능
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

    def main(args: Array[String]): Unit = {
        val list = List(1,2,3)

        // println(List(1,2,3))

        println(drop(list, 1))

        // println(foldRight(list, 3))
        println(sumFoldLeft(list))
    }
}
