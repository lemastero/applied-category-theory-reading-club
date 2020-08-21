object StdCat:

  /* Sum type custom version
   *
   * we need our tagged type for sums, because
   * scala3 union types are actually untagged
   * and the original type can't be pattern matched
   * on. I.e. once you have a x: A|B we're not able
   * to figure out anymore which side x was on
   */
  enum <+>[+A, +B]:
    case <+[A, B](inl: A) extends (A <+> B)
    case +>[A, B](inr: B) extends (A <+> B)

    def swap: B <+> A = this match
    case <+(inl) => +>(inl)
    case +>(inr) => <+(inr)

  extension [A, B](a: A):
    def inl  = <+>.<+[A, B](a)
    def inr = <+>.+>[B, A](a)

  /**********************
   * Definitions
   **********************/

  trait Functor[F[_]]:
    def fmap[A, B](f: A => B)(fa: F[A]): F[B]

  trait ProFunctor[|=>[_, _]]:
    def dimap[A, B, C, D](l: C => A)(r: B => D)(p: A |=> B): C |=> D

  trait Cartesian[|=>[_, _]] extends ProFunctor[|=>]:
    def first[A, B, C](p: A |=> B): ((A, C)) |=> (B, C) =
      dimap[(C, A), (C, B), (A, C), (B, C)](_.swap)(_.swap)(second[A, B, C](p))

    def second[A, B, C](p: A |=> B): ((C, A)) |=> (C, B)

  trait CoCartesian[|=>[_, _]] extends ProFunctor[|=>]:
    def left[A, B, C](p: A |=> B): (A <+> C) |=> (B <+> C) =
      dimap[C <+> A, C <+> B, A <+> C, B <+> C](_.swap)(_.swap)(right[A, B, C](p))

    def right[A, B, C](p: A |=> B): (C <+> A) |=> (C <+> B)

  /* Seems like we can't do without a Nominal Type
   * a type alias wouldn't work
   */
  trait BiCartesian[|=>[_, _]] extends Cartesian[|=>] with CoCartesian[|=>]

  /**********************
   * Identity Functor
   **********************/

  type Id = [A] =>> A

  given as Functor[Id]:
    override def fmap[A, B](f: A => B)(a: A): B = f(a)

  /**********************
   * Instances for A => B
   **********************/
  object ProFunctor:
    /* Using an intersection type here would add an Object to the
     * final join type, it seems, so we go for a nominal type, using
     * a custom trait
     */
    given BiCartesian[Function] =
      new BiCartesian:
        import <+>._

        override def dimap[A, B, C, D](l: C => A)(r: B => D)(p: A => B): C => D =
          r compose p compose l

        override def second[A, B, C](p: A => B): ((C, A)) => (C, B) =
          {case ((c, a)) => (c, p(a))}

        override def right[A, B, C](p: A => B): (C <+> A) => (C <+> B) =
          case <+(c: C) => c.inl
          case +>(a: A) => p(a).inr

end StdCat

object Existentials:
  /* Inspired from the work for scala 2 in
   * https://github.com/djspiewak/skolems
   */

  /* An existential for a type X
   * with dependent type encoding
   */
  trait Exists[F[_]]:
    type X
    val f: F[X]

  /* instance of an existential as a polymorphic given */
  given [F[_], A](using F[A]) as Exists[F]:
    type X = A
    val f = summon[F[A]]

  object Exists:
    def apply[F[_], A](f: F[A]): Exists[F] =
      given F[A] = f
      summon[Exists[F]]