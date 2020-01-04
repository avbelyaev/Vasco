package ru.avbelyaev.vasco

/**
 * @author avbelyaev
 */
object ClosureConverter {

  def convertClosures(exp: Exp): Exp = exp.exp match {
    case "lambda" :: params :: body => Exp.of(body)
    case _ => throw new IllegalArgumentException("nil exp")
  }
}
