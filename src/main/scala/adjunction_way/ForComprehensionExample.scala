package adjunction_way

import java.time.LocalDateTime

//  [ (cust.name, inv.amount) |
//    cust <- customers,
//    inv <- invoices,
//    cust.cust_id = inv.cust_id,
//    inv.due < now()
//  ]
object ForComprehensionExample extends App {
  case class Customer(id: Long, name: String)
  case class Invoice(custId: Long, dueDate: LocalDateTime)

  val customers = Set.empty[Customer]
  val invoices = Set.empty[Invoice]
  val now = LocalDateTime.now()
  for {
    c <- customers
    i <- invoices if c.id == i.custId && i.dueDate.isBefore(now)
  } yield (c, i)
}
