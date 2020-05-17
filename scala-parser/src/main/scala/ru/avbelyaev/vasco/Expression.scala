package ru.avbelyaev.vasco

import scala.collection.mutable.ListBuffer

/**
 * @author avbelyaev
 */
object Expression {

  def of(expression: Object): Expression = {
    expression match {
      case exp: Expression => exp
      case expressions: List[Object] => Expression.of(expressions.asInstanceOf[List[Object]])
      case _ => throw new UnexpectedTypeException(expression)
    }
  }

  def of(expressions: List[Object]): Expression = {
    var es = new ListBuffer[Object]
    expressions.foreach {
      case exp@(_: List[Object]) => es += Expression.of(exp)
      case exp@(_: String) => es += exp
      case e => throw new UnexpectedTypeException(e)
    }
    new Expression(es.toList)
  }

  def empty(): Expression = new Expression(List())
}


class Expression private(exprs: List[Object]) {

  val exp: List[Object] = exprs

  override def toString: String = {
    val sb = new StringBuilder("(")
    this.exp.foreach {
      case exp@(_: Expression) => sb append s"${exp.toString()} "
      case exp@(_: String) => sb append s"$exp "
      case exp => throw new IllegalArgumentException(s"Invalid type: ${exp.getClass}")
    }
    sb append ")"
    sb.toString().replaceAll(" \\)", ")")
  }
}


