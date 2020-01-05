package ru.avbelyaev.vasco

import java.util.UUID

import com.typesafe.scalalogging.Logger

/**
 * @author avbelyaev
 */
object ClosureConverter {

  def convertClosure(exp: Exp): Closure = exp match {
    case "lambda" :: params :: body => new Closure(exp, params, body)
    case _ => throw new IllegalArgumentException("nil exp")
  }
}


object Closure {

  private val log = Logger("clo")

//  def str(xs: List[_]): String = xs.mkString("(", " ", ")")

  @scala.annotation.tailrec
  def vars(exp: Exp): Set[String] = exp match {
    case "lambda" :: params :: body =>
      log.debug("params: {}, body: {}", params, body)
      vars(body.head.asInstanceOf[Exp])
    case (func: String) :: (args: List[String]) =>
      log.debug("func: {}, args: {}", func, args)
      Set(func) ++ args
  }

  def freeVars(body: Exp): Set[String] = body match {
      // [`(lambda ,params ,body)
    //     (set-subtract (free body) (apply set params))]
    case "lambda" :: (params: List[String]) :: body =>
      log.debug("lambda params: {} body: {}", params, body)
      freeVars(body.head.asInstanceOf[Exp]) -- params
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
  //  def freeVars(params: Exp, body: Exp): List[String] = {
  //    var s = new Set[Object].empty
  //
  //    var _body = body.head.asInstanceOf[List[Object]]
  //    _body = body match {
  //      case "lambda" :: params :: body => throw new NotImplementedException("lambda in free var detection si not handled")
  //      case func: String :: args =>
  //      case _ => throw new UnexpectedTypeException(params)
  //    }
  //    if (1 != _body.exp.size) {
  //      throw new IllegalArgumentException(s"body ${_body} size expected: 1, actual: ${_body.exp.size}")
  //    }
  //
  //
  //  }
}

// TODO try implicit conversion Object -> Expression
class Closure(exp: Object, params: Object, body: Object) {
  val id: String = UUID.randomUUID().toString
  //  val freeVars: List[String] = Closure.freeVars(params.asInstanceOf[List[Object]], body.asInstanceOf[List[Object]])
  val name = "fuck"

  override def toString: String = "Closure {" + this.name + " " + this.body + "}"
}

