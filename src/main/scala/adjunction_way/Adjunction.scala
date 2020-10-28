package adjunction_way

trait Adjunction1[F[_], G[_]] {
  def unit[A]: A => G[F[A]]
  def counit[B]: F[G[B]] => B
}

trait Adjunction1Laws[F[_], G[_]] extends Adjunction1[F, G] {
  val FF: Functor[F]
  val FG: Functor[G]

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

trait Adjunction2[F[_], G[_]] extends Adjunction1[F,G]{
  def left[X, Y](f: F[X] => Y): X => G[Y]
  def right[X, Y](f: X => G[Y]): F[X] => Y

  override def unit[A]: A => G[F[A]] = left(identity)
  override def counit[B]: F[G[B]] => B = right(identity)
}

trait Adjunction2From1[F[_], G[_]] extends Adjunction1[F,G]{
  val FF: Functor[F]
  val FG: Functor[G]

  def left[X, Y](f: F[X] => Y): X => G[Y] = a => FG.map(unit(a))(f)
  def right[X, Y](f: X => G[Y]): F[X] => Y = a => counit(FF.map(a)(f))
}

trait Adjunction2Laws[F[_], G[_]] extends Adjunction2[F,G] {
  val FF: Functor[F]
  val FG: Functor[G]

  // laws from category theory

  def naturalityOnX[X,Y](fn: F[X] => Y, contr: F[X] => X): Boolean = {
    val rhs1: X => G[Y] = left(fn)
    val rhs2: F[X] => G[Y] = contr andThen rhs1

    val lhs1: F[F[X]] => Y = ffx => fn(FF.map(ffx)(contr))
    val lhs2: F[X] => G[Y] = left(lhs1)
    rhs2 == lhs2
  }

  def naturalityOnY[X,Y](fn: X => G[Y], contr: Y => G[Y]): Boolean = {
    val rhs1: F[X] => Y = right(fn)
    val rhs2: F[X] => G[Y] = rhs1 andThen contr

    val lhs1: X => G[G[Y]] = x => FG.map(fn(x))(contr)
    val lhs2: F[X] => G[Y] = right(lhs1)
    rhs2 == lhs2
  }

  // laws from FP

  def adjunctionLaw1[X,Y](fn: F[X] => Y): Boolean = {
    val rhs1: X => G[Y] = left(fn)
    val rhs2: F[X] => Y = right(rhs1)
    fn == rhs2
  }

  def adjunctionLaw2[X,Y](fn: X => G[Y]): Boolean = {
    val rhs1: F[X] => Y = right(fn)
    val rhs2: X => G[Y] = left(rhs1)
    fn == rhs2
  }
}

object AdjunctionExamples {
  def tupleFunctionFromAdjunction[R]: Adjunction2[Tuple2[*, R], Function1[R, *]] =
    new Adjunction2[(*, R), R => *] {
      def left[A, B](f: ((A, R)) => B): A => R => B =
        Function.untupled(f).curried

      def right[A, B](f: A => R => B): ((A, R)) => B =
        Function.uncurried(f).tupled
    }
}