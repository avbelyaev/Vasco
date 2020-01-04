package ru.avbelyaev.vasco

import scala.collection.mutable.ListBuffer

/**
 * @author avbelyaev
 */
object Exp {

  def of(expressions: List[Object]): Exp = {
    var es = new ListBuffer[Object]
    expressions.foreach { e =>
      //      println(e)
      e match {
        case exp@(_: List[Object]) => es += Exp.of(exp)
        case exp@(_: String) => es += exp
      }
    }
    new Exp(es.toList)
  }
}

class Exp private(exprs: List[Object]) {

  val exp: List[Object] = exprs

  override def toString: String = {
    val sb = new StringBuilder("(")
    this.exp.foreach {
      case exp@(_: Exp) => sb append s"${exp.toString()} "
      case exp@(_: String) => sb append s"$exp "
      case exp => throw new IllegalArgumentException(s"Invalid type: ${exp.getClass}")
    }
    sb append ")"
    sb.toString().replaceAll(" \\)", ")")
  }
}


