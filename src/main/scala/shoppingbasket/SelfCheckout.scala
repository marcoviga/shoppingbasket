package shoppingbasket
import scala.math.BigDecimal.RoundingMode

object SelfCheckout {


  case class Apple(price: BigDecimal) extends Item
  case class Orange(price: BigDecimal) extends Item
  sealed trait Item { val price: BigDecimal }

  val getBasketPrice: List[Item] => BigDecimal = (list: List[Item]) => {
    list.foldLeft(BigDecimal(0))((acc, item) => acc + item.price)
  }

  def oneItemForFreeEachNth(n: Int)(partition: (List[Item]) => (List[Item], List[Item]))(list: List[Item]): List[Item] = {
    val l: (List[Item], List[Item]) = partition(list)

    val filtered = l._1.indices.map(i => i + 1).collect { case i if i % n != 0 => l._1(i - 1) }.toList

    filtered ++ l._2
  }

  val oneOrangeForFree: (List[Item]) => List[Item] = oneItemForFreeEachNth(3)(partionByOranges)
  val oneAppleForFree: (List[Item]) => List[Item] = oneItemForFreeEachNth(2)(partionByApples)
  val oneItemForFree: (List[Item]) => List[Item] = oneItemForFreeEachNth(3)(partionByItem)

  lazy val partionByOranges: (List[Item]) => (List[Item], List[Item]) = (list: List[Item]) => list.partition(_.isInstanceOf[Orange])
  lazy val partionByApples: (List[Item]) => (List[Item], List[Item]) = (list: List[Item]) => list.partition(_.isInstanceOf[Apple])
  lazy val partionByItem: (List[Item]) => (List[Item], List[Item]) = (list: List[Item]) => list.partition(_.isInstanceOf[Item])


}
