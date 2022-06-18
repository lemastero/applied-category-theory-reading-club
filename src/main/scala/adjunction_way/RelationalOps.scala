package adjunction_way

import java.time.LocalDateTime

import RelationalAlgebra.{Bag, Key}
import cats.{Monad, StackSafeMonad}

abstract class RelationalAlgebra[V] {
  def indexBy[K: Key]:   Bag[V] => (V => K) => (K => Bag[V])
  def merge[K: Key, V1]: (K => V, K => V1) => (K => (V,V1))
  def cod[K: Key]:       K => Bag[V] => Bag[V]
}

object RelationalAlgebra {
  type Key[K] = K
  trait Bag[V]

  val functor: Monad[RelationalAlgebra] = new StackSafeMonad[RelationalAlgebra] {
    override def flatMap[A, B](fa: RelationalAlgebra[A])(f: A => RelationalAlgebra[B]): RelationalAlgebra[B] = ???
    override def pure[A](x: A): RelationalAlgebra[A] = ???
  }
}

object Example {
  case class Customer(id: Long, name: String)
  case class Invoice(custId: Long, dueDate: LocalDateTime)

  val now: LocalDateTime = LocalDateTime.now()

  // fmap(fmap name x fmap amount) (cod (
  //  fmap (id x filter (λi -> i.due < now)) (
  //    merge (customers `indexBy` cid, invoices `indexBy` cust)
  //  )
  // ))
//  def query(
//             customers: Bag[Customer],
//             invoices: Bag[Invoice],
//             now: LocalDateTime,
//             calgebra: RelationalAlgebra[Customer],
//             ialgebra: RelationalAlgebra[Invoice]) = {
//    def cusId: Customer => Long = _.id
//    def foo2: Long => Bag[Customer] = calgebra.indexBy(customers)(cusId)
//    merge()
//  }
}
