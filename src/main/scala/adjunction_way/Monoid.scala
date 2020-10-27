package adjunction_way

trait Semigroup[M] {
  def combine(lhs: M, rhs: M): M
}

// TODO Semigroup laws

trait Monoid[M] extends Semigroup[M] {
  def empty: M
}

// TODO Monoid laws

trait CommutativeMonoid[M] extends Monoid[M]

// TODO CommutativeMonoid laws