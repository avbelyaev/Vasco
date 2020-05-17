package ru.avbelyaev.vasco

import org.scalatest.funsuite.AnyFunSuite

/**
 * @author avbelyaev
 */
//It is not possible to run single test with FlatSpec tests
class ClosureTests extends AnyFunSuite {

  test("should find free vars in expression") {
    var actual = Closure.freeVars(
      List("lambda", List("x"),
        List("+", "x", "a", "b")))

    assert(actual == Set("+", "a", "b"))
  }

  test("should convert expression into closure") {
    val actual = new Closure(
      List("lambda", List("f"),
        List("lambda", List("x"),
          List("f", "x", "a"))), null)

    assert(actual.func.isInstanceOf[Either[Exp, Closure]])
    assert(actual.func.isRight)
  }
}
