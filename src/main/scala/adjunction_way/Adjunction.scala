package adjunction_way

object Definition1 {

  trait Adjunction[F[_], G[_]] {
    def unit[A]: A => G[F[A]]
    def counit[B]: F[G[B]] => B

    val FF: Functor[F]
    val FG: Functor[G]

    def left[X, Y](f: F[X] => Y): X => G[Y] = a => FG.map(unit(a))(f)
    def right[X, Y](f: X => G[Y]): F[X] => Y = a => counit(FF.map(a)(f))
  }

  trait AdjunctionLaws[F[_], G[_]] extends Adjunction[F, G] {
    def triangleIdentityF[A](fa: F[A]): Boolean = {
      val rhs1: F[G[F[A]]] = FF.map(fa)(unit)
      val rhs2: F[A] = counit(rhs1)
      fa == rhs2
    }

    def triangleIdentityG[A](fa: G[A]): Boolean = {
      val rhs1: G[F[G[A]]] = unit(fa)
      val rhs2: G[A] = FG.map(rhs1)(counit)
      fa == rhs2
    }
  }
}

object Definition2 {

  trait Adjunction[F[_], G[_]] {
    def left[X, Y](f: F[X] => Y): X => G[Y]
    def right[X, Y](f: X => G[Y]): F[X] => Y

    def unit[A]: A => G[F[A]] = left(identity)
    def counit[B]: F[G[B]] => B = right(identity)
  }

//  trait AdjunctionLaws[F[_], G[_]] extends Adjunction[F,G] {
//    def naturalityOnX[X,Y](fn: F[X] => Y): Boolean = {
//      val rhs1: X => G[Y] = left(fn)
//      val rhs2: F[X] => G[Y] = ???
//      val lhs1: F[F[X]] => Y = ???
//      val lhs2: F[X] => G[Y] = left(lhs1)
//      ???
//    }
//  }

  def tupleFunctionFromAdjunction[R]: Adjunction[Tuple2[*, R], Function1[R, *]] =
    new Adjunction[(*, R), R => *] {
      def left[A, B](f: ((A, R)) => B): A => R => B =
        Function.untupled(f).curried
      def right[A, B](f: A => R => B): ((A, R)) => B =
        Function.uncurried(f).tupled
    }
}
