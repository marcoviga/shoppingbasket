package shoppingbasket

import org.scalatest.FunSuite
import shoppingbasket.SelfCheckout.{Apple, Orange}
import shoppingbasket.SelfCheckout.{getBasketPrice, oneAppleForFree, oneItemForFree, oneOrangeForFree}

class SelfCheckoutSpec extends FunSuite {

  test("getBasketPrice with no discount and no elements") {
    val input = List()

    assert(getBasketPrice(input).equals(BigDecimal(0)))
  }

  test("getBasketPrice with no discount") {
    val input = List(Apple(BigDecimal(0.60)), Apple(BigDecimal(0.60)), Orange(BigDecimal(0.25)))
    assert(getBasketPrice(input).equals(BigDecimal(1.45)))
  }

  test("getBasketPrice with apples discount") {
    val input = List(Apple(BigDecimal(0.60)), Apple(BigDecimal(0.60)), Apple(BigDecimal(0.60)))

    val discountFunc = oneAppleForFree andThen getBasketPrice
    assert(discountFunc(input) == 1.20)
  }

  test("getBasketPrice with oranges discount") {
    val input = List(Orange(BigDecimal(0.25)), Orange(BigDecimal(0.25)), Orange(BigDecimal(0.25)))

    val discountFunc = oneOrangeForFree andThen getBasketPrice
    assert(discountFunc(input) == 0.50)
  }

  test("getBasketPrice with every three element discount") {
    val input = List(Orange(BigDecimal(0.25)), Apple(BigDecimal(0.6)), Orange(BigDecimal(0.25)))

    val discountFunc = oneItemForFree andThen getBasketPrice
    assert(discountFunc(input) == 0.85)
  }

  test("getBasketPrice with apple and oranges discount") {
    val input = List(Orange(BigDecimal(0.25)), Orange(BigDecimal(0.25)), Orange(BigDecimal(0.25)),
      Apple(BigDecimal(0.60)), Apple(BigDecimal(0.60)))

    val discountFunc = oneAppleForFree andThen oneOrangeForFree andThen getBasketPrice
    assert(discountFunc(input) == 1.10)
  }

}