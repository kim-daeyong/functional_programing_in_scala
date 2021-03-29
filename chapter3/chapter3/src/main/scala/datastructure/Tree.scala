package datastructure

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

    def size[A](tree: Tree[A]): Int = tree match{
        case Leaf(_) => 1
        case Branch(left, right) => size(left) + size(right) + 1
    }

    def maximum(tree: Tree[Int]): Int = tree match {
    
        case Leaf(value) => value
        case Branch(left, right) =>  maximum(left).max(maximum(right))
    }

    def depth[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 0
        case Branch(left, right) => depth(left).max(depth(right)) + 1
    }

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {

        case Leaf(value) => Leaf(f(value))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

    def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {

        case Leaf(value) => f(value)
        case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }

    def flatMap[A,B](tree: Tree[A])(f: A => Tree[B]): Tree[B] = tree match {

        case Branch(left, _) => flatMap(left)(f)
        case Leaf(value) => f(value)
        case Branch(left, _) => flatMap(left)(f)
        case Branch(_, right) => flatMap(right)(f)
    }
    
        def main(args: Array[String]): Unit = {
        val tree = Branch(
                        Branch(
                        Leaf(12),
                            Branch(
                                Leaf(3),
                                Leaf(4))),
                            Leaf(8))

        println(tree)
        println(size(tree))
        println(maximum(tree))
        println(depth(tree))
        println(map(tree)(x => x+1))
        println()
    }

}
