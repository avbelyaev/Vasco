package ru.avbelyaev.vasco

import org.scalatest.flatspec.AnyFlatSpec

/**
 * @author avbelyaev
 */
class ClosureTests extends AnyFlatSpec {

  it should "find all vars in expression" in {
    val e = List("lambda", List("x"), List("+", "x", "a", "b"))

    val actual = Closure.vars(e)

    val expected = Set("+", "a", "b", "x")
    assert(actual == expected)
  }

  it should "find free vars in expression" in {
    val e = List("lambda", List("x"), List("+", "x", "a", "b"))

    val actual = Closure.freeVars(e)

    val expected = Set("+", "a", "b")
    assert(actual == expected)
  }
}
