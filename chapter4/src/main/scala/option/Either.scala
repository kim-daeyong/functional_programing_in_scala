package option


// option은 에러를 숨기고 다른값을 준다.

/*
1.
    add(m: Int):
        return n+1: Int
    Option.lift(add)(Some(3)).getOrElse()
    
    add의 정상값은 뭘까? 에러를 할때 -1등의 디폴트 값이 필요하다.
    c의 경우 정상종료는 0, 비정상 1,2등

    에러라는걸 알고싶다.
2.
    error ->  I/O에러, Memory Overflow, ~~~ 등등 에러의 종류를 알아야한 필요가 있다.

    이러한 것을 인터페이스

    정상은 right
    비정상은 left

interface 가 먼저!


콘크리트와 하이오더펑션을 이용한 구현의 차이는??

either의 flatmap > 라이트를 레프트로 변경가능 > id에선 정상인데 post에서 비정상이라면 그 값을 비정상으로.

map 정상 밸류를 > 가공

sequence는 여러개의 익셉션을 하나의 결과물로 바꿀 수 있다.

traverse 아무것도 아닌 조합을 op을 씌우고 싶다. 전부다 반복적인 일에 동일한 처리를 할 수 있다.

지금까지 의 경험에서 에러 처리는 이정도면 충분한가?

java 

try {

} catch {

} catch {
    에러 종류가 다를경우 다른 액션은 아직 불가 (sequence1 까지 했을떄) 둘중에 하나의 에러만 처리된다: (issue)
} finally {
    가능
}

issue
    여러 에러 종류를 처리하는법?

    1. new op? 새로운 op
    2. new structure? either의 구조
    3. new structure using Either?

*/
sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
        case Left(value) => Left(value)
        case Right(value) => Right(f(value))
    }
    // B >: A  B가 A의 상위 <: 반대 
    def flatMap[EE >: E,B](f: A => Either[EE, B]): Either[EE, B] = this match {
        case Left(value) => Left(value)
        case Right(value) => f(value)
    }

    def orElse[EE >: E, B, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = this match {
        case Left(value) => b
        case Right(value) => Right(value)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] = 
        this.flatMap(a => b.map(bb => f(a, bb)))
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

    def Try[A](a: => A): Either[Exception, A] =
        try Right(a)
        catch { 
            case e: Exception => Left(e)
        }
    
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
        case Nil => Right(Nil)
        case h::t => h.map2(sequence(t))((hh,tt) => hh::tt)
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
        sequence(as.map(a => f(a)))

    def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
        // traverse(es)(a => a)
        traverse(es)(identity)// x를 받아서 x를 내보낸다.
}