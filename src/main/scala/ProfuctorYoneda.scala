import StdCat._
import Existentials._

object ProYoneda:

  /**********************
   * Lenses
   **********************/
  case class Lens[A, B, S, T](
    view: S => A,
    update: (S, B) => T
  )

  type MonoLens[A, S] = Lens[A, A, S, S]

  val sndLens:[A, B, C] => () => Lens[A, B, (C,A), (C,B)] =
    [A, B, C] => () =>
      val vw: ((C, A)) => A = _._2
      val up: ((C, A), B) => (C, B) = (ca, b) => ca._1 -> b
      Lens(vw, up)

  /**********************
   * Prisms
   **********************/
  case class Prism[A, B, S, T](
    matches: S => T | A,
    build: B => T
  )

  type MonoPrism[A, S] = Prism[A, A, S, S]

  def the[A, B]: Prism[A, B, Option[A], Option[B]] =
    Prism(
      {
        case Some(a) => a
        case _ => None
      },
      Some[B]
    )

  /* Sadly the poly-function value version traps some dotty bug
   * [error] -- [E007] Type Mismatch Error: /Users/scalac/Documents/projects/prof-optics-dotty/profunctor-optics/src/main/scala/ProfuctorYoneda.scala:38:7
   * [error] 38 |      )
   * [error]    |       ^
   * [error]    |Found:    Object with PolyFunction {...}
   * [error]    |Required: PolyFunction{apply: [A, B](): ProYoneda.Prism[A, B, Option[A], Option[B]]}
   */
  // val the:[A, B] => () => Prism[A, B, Option[A], Option[B]] =
  //   [A, B] => () =>
  //     Prism(
  //       {
  //         case Some(a) => a
  //         case _ => None
  //       },
  //       Some[B]
  //     )

  /**********************
   * Adapters
   **********************/
  case class Adapter[A, B, S, T](
    from: S => A,
    to: B => T
  )

  type MonoAdapter[A, S] = Adapter[A, A, S, S]

  val flatten: [A, B, C, X, Y, Z] => () => Adapter[(A, B, C), (X, Y, Z), ((A, B), C), ((X, Y), Z)] =
    [A, B, C, X, Y, Z] => () =>
      val f: (((A, B), C)) => (A, B, C) =
        case ((a, b), c) => (a, b, c)
      val t: ((X, Y, Z)) => ((X, Y), Z) =
        case (a, b, c) => ((a, b), c)
      Adapter(f, t)

  /**********************
   * Profunctor versions
   * Adapters
   **********************/

  trait AdapterP[A, B, S, T]:
    def apply[|=>[_, _]: ProFunctor]: (A |=> B) => (S |=> T)

  def flattenP[A, B, C, X, Y, Z]: AdapterP[(A, B, C), (X, Y, Z), ((A, B), C), ((X, Y), Z)] =
      new AdapterP:
        type I = (A, B, C)
        type O = (X, Y, Z)
        type S = ((A, B), C)
        type T = ((X, Y), Z)
        override def apply[|=>[_, _]: ProFunctor]: (I |=> O) => (S |=> T) = h =>
          val f: S => I =
            case ((a, b), c) => (a, b, c)
          val t: O => T =
            case (a, b, c) => ((a, b), c)
          summon[ProFunctor[|=>]].dimap(f)(t)(h)

  /* Once more can't express this with a PolyFunction
   * [error]    |Found:    Object with PolyFunction {...}
   * [error]    |Required: PolyFunction{
   * [error]    |  apply:
   * [error]    |    [A, B, C, X, Y, Z]
   * [error]    |      (): ProYoneda.AdapterP[(A, B, C), (X, Y, Z), ((A, B), C), ((X, Y), Z)]
   * [error]    |}
   */
  /*
   val flattenP: [A, B, C, X, Y, Z] => () => AdapterP[(A, B, C), (X, Y, Z), ((A, B), C), ((X, Y), Z)] =
    [A, B, C, X, Y, Z] => () =>
      new AdapterP:
        type I = (A, B, C)
        type O = (X, Y, Z)
        type S = ((A, B), C)
        type T = ((X, Y), Z)
        override def apply[|=>[_, _]: ProFunctor]: (I |=> O) => (S |=> T) = h =>
          val f: S => I =
            case ((a, b), c) => (a, b, c)
          val t: O => T =
            case (a, b, c) => ((a, b), c)
          summon[ProFunctor[|=>]].dimap(f)(t)(h)
    */

  /**********************
   * Profunctor versions
   * Lenses
   **********************/

  trait LensP[A, B, S, T]:
    def apply[|=>[_, _]: Cartesian]: (A |=> B) => (S |=> T)

  def sndLensP[A, B, C]: LensP[A, B, (C, A), (C, B)] =
    new LensP:
      override def apply[|=>[_, _]: Cartesian] = summon[Cartesian[|=>]].second

  /**********************
   * Profunctor versions
   * Prisms
   **********************/

  trait PrismP[A, B, S, T]:
    def apply[|=>[_, _]: CoCartesian]: (A |=> B) => (S |=> T)

  def theP[A, B]: PrismP[A, B, Option[A], Option[B]] =
    new PrismP:
      override def apply[|=>[_, _]](using coc: CoCartesian[|=>]) = h =>
        import StdCat.<+>._
        val coc = summon[CoCartesian[|=>]]
        import coc._
        val fromOpt: Option[A] => Unit <+> A =
          case None    => ().inl
          case Some(x) => x.inr
        val toOpt: Unit <+> B => Option[B] =
          case <+(()) => Option.empty
          case +>(x)  => Some(x)
        dimap(fromOpt)(toOpt)(right(h))

  /**********************************
   * Symmetric form of Lens category
   **********************************/

  case class LensArrow[A, B, S, T](lens: Exists[[C] =>> (S => (C, A), ((C, B)) => T)])

  /**********************************
   * Symmetric form of Prism category
   **********************************/

  case class PrismArrow[A, B, S, T](prism: Exists[[C] =>> (S => (C <+> A), (C <+> B) => T)])

end ProYoneda

object Yoneda:

  /**********************
   * Lemma
   **********************/
  case class Yo[F[_], A](unYo: [R] => (A => R) => F[R])

  //fromYo: [F[_], A] => Yo[F, A] => F[A]
  val fromYo =
    [F[_], A] => (y: Yo[F, A]) => y.unYo(identity)

  // toYo: [F[_], A] => Functor[F] ?=> F[A] => Yo[F, A]
  val toYo =
    [F[_], A] => (using F: Functor[F]) =>
        (x: F[A]) => Yo[F, A]([R] => (h: A => R) => F.fmap(h)(x))

  /**********************
   * Lemma [Dual] BROKEN!
   **********************/
  /* Implementation restriction: polymorphic function types must have a value parameter */
  case class CoYoP[F[_], R](unCoYo: [C] => () => (F[C], C => R))

  /* fromCoYoP: [F[_], B] => Functor[F] ?=> CoYo[F, B] => F[B] */
  val fromCoYoP =
    [F[_], B] => (using F: Functor[F]) =>
      (cy: CoYoP[F, B]) =>
        val (x, h) = cy.unCoYo()
        F.fmap(h)(x)

   /*
   * Universal encoding like this won't work to represent
   * the F => CoYo direction, since we need to provide a
   * pair (F[A], A => R) for any A
   *
   * Instead we want an existential, which means that the
   * implementation decides the A and not the caller
   * Only in this way we can fix the A
   * to the passed-in B in the toCoYo direction
   */
  // val toCoYoP: [F[_], B] => F[B] => CoYoP[F, B] =
  //   [F[_], B] => (y: F[B]) => CoYoP([C] => () => what here ???)


  /****************************
   * Lemma [Dual with Exists]
   ****************************/
  case class CoYo[F[_], R](unCoYo: Exists[[C] =>> (F[C], C => R)])

  /* fromCoYo: [F[_], B] => Functor[F] ?=> CoYo[F, B] => F[B] */
  val fromCoYo =
    [F[_], B] => (using F: Functor[F]) =>
      (cy: CoYo[F, B]) =>
        val (f, h) = cy.unCoYo.f
        F.fmap(h)(f)

  /* toCoYo: [F[_], B] => F[B] => CoYo[F, B] */
  val toCoYo =
    [F[_], B] => (y: F[B]) =>
      CoYo[F, B](Exists(y -> identity[B]))

  object CoYo:
    /* coyo functor instance */
    given [F[_]] as Functor[[R] =>> CoYo[F, R]] = new Functor:
      override def fmap[A, B](g: A => B)(y: CoYo[F, A]) =
        val (x, h) = y.unCoYo.f
        CoYo[F, B](Exists(x -> (g compose h)))

  /****************************
   * Double Yoneda Embedding
   ****************************/

  /* A => B <=> for any functor. F[A] => F[B] */

  def fromFun[A, B](f: A => B): [F[_]] => Functor[F] ?=> F[A] => F[B] =
    [F[_]] => (using F: Functor[F]) => F.fmap(f)

  def toFun[A, B](h: [F[_]] => Functor[F] ?=> F[A] => F[B]): (A => B) =
    h[Id]


trait UniversalPropOfExistential[B, F[_]]:

  def f_universal: [A] => F[A] => B

  /* unreducible application of higher-kinded type F to wildcard arguments */
  // def f_existential: F[?] => B
  def f_existential: Exists[[X] =>> F[X] => B]

object Unilist extends UniversalPropOfExistential[Int, List]:

  override val f_universal: [A] => List[A] => Int =
    [A] => (l: List[A]) => l.size

  override val f_existential = new Exists:
    type X = Any
    val f = _.size

