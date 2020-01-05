package ru.avbelyaev.vasco

import java.util.UUID

import com.typesafe.scalalogging.Logger

/**
 * @author avbelyaev
 */
object Closure {

  private val log = Logger("clo")

  def freeVars(e: Exp): Set[String] =
    e match {
      // [`(lambda ,params ,body)
      //     (set-subtract (free body) (apply set params))]
      case "lambda" :: exp =>
        val lambdaArgs = exp.head.asInstanceOf[List[String]]
        val lambdaExpr = exp.tail.head.asInstanceOf[Exp]
        log.debug("lambda args: {} body: {}", lambdaArgs, lambdaExpr)
        freeVars(lambdaExpr) -- lambdaArgs
      // [(? symbol?)
      //     (set exp)]
      case (symbol: String) :: Nil =>
        log.debug("symbol: {}", symbol)
        Set(symbol)
      // [`(,f ,args ...)
      //     (apply set-union (map free `(,f . ,args)))]))
      case funcWithArgs: List[String] =>
        log.debug("func&args: {}", funcWithArgs)
        funcWithArgs.toSet
      // TODO case f[List[_]], args[List[_]]
    }


  def closureConvert(exp: Exp): Closure =
    exp match {
      case "lambda" :: (params: List[String]) :: body =>
        new Closure(exp, null)
    }
}

//class Closure(exp: Exp,
//              params: List[String],
//              _body: Exp,
//              parent: Closure) {
class Closure(exp: Exp, parent: Closure) {
  val _exp = exp

  // top-down closure convert
  val func: Either[Exp, Closure] =
    exp match {
      case "lambda" :: (params: List[String]) :: body =>
        Right(new Closure(body.head.asInstanceOf[Exp], this))
      case _ =>
        Left(exp)
    }
  val id: String = UUID.randomUUID().toString
  var freeVars: Set[String] = Closure.freeVars(this._exp)

  override def toString: String = "Closure {" + this.id + "}"

  override def equals(obj: Any): Boolean = this.id.equals(obj.asInstanceOf[Closure].id)

  override def hashCode(): Int = this.id.hashCode
}

