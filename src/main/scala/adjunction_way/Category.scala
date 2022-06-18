package adjunction_way

trait Semicategory[X, Morphism[_ <: X, _ <: X]] {
  def compose[A <: X, B <: X, C <: X](f: Morphism[B, C])(g: Morphism[A, B]): Morphism[A, C]
}

trait SemicategoryLaws[X, M[_ <: X, _ <: X]] extends Semicategory[X, M] {

  def compositivityLaw[A <: X, B <: X, C <: X, D <: X](g: M[B, C], f: M[A, B], h: M[C, D]): Boolean = {
  val gf: M[A, C] = compose(g)(f)
  val v2: M[A, D] = compose(h)(gf)

  val hg: M[B, D] = compose(h)(g)
  val w2: M[A, D] = compose(hg)(f)

  v2 == w2
  }
}

trait Category[X, Morphism[_  <: X, _  <: X]] extends Semicategory[X, Morphism] {
  def id[Obj <: X]: Morphism[Obj, Obj]
}

trait CategoryLaws[X, M[_ <: X, _ <: X]] extends Category[X, M] {
  def leftIdentityLaw[A <: X, B <: X](fa: M[A, B]): Boolean = {
    compose(id[B])(fa) == fa
  }

  def rightIdentityLaw[A <: X, B <: X](fa: M[A, B]): Boolean = {
    compose(fa)(id[A]) == fa
  }
}

object Category {
  trait Function1Cat extends Category[Any, Function1] {
    def id[A]: A => A = identity[A]
    def compose[A, B, C](f: B => C)(g: A => B): A => C = g andThen f
  }

  val scalaProperTypesAndPureFunction1: Category[Any, Function1] =
    new Function1Cat {}

  def Mon[M, MM : Monoid]: Category[Monoid[M], Function1] = new Category[Monoid[M], Function1] {

    override def id[Obj <: Monoid[M]]: Obj => Obj =
      identity[Obj]

    override def compose[A <: Monoid[M], B <: Monoid[M], C <: Monoid[M]](f: B => C)(g: A => B): A => C =
      f compose g
  }

  def CMon[M, MM : CommutativeMonoid]: Category[CommutativeMonoid[M], Function1] = new Category[CommutativeMonoid[M], Function1] {

    override def id[Obj <: CommutativeMonoid[M]]: Obj => Obj =
      identity[Obj]

    override def compose[A <: CommutativeMonoid[M], B <: CommutativeMonoid[M], C <: CommutativeMonoid[M]](f: B => C)(g: A => B): A => C =
      f compose g
  }
}
