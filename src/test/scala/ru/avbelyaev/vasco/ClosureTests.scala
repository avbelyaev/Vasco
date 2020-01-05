package ru.avbelyaev.vasco

import org.scalatest.flatspec.AnyFlatSpec

/**
 * @author avbelyaev
 */
class ClosureTests extends AnyFlatSpec {

  it should "find all vars in expression" in {
    val e = List("lambda", List("x"), List("+", "x", "a", "b"))

    val actualFree = Closure.vars(e)

    val expectedFree = Set("+", "a", "b", "x")
    assert(expectedFree == actualFree)
  }
}
