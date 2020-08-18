object ProYoneda:

  import StdCat._

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

end ProYoneda

trait UniversalPropOfExistential[B, F[_]]:

  def f_universal: [A] => F[A] => B

  /* unreducible application of higher-kinded type F to wildcard arguments */
  // def f_existential: F[?] => B

object Unilist extends UniversalPropOfExistential[Int, List]:

  override val f_universal: [A] => List[A] => Int =
    [A] => (l: List[A]) => l.size
