package ru.avbelyaev.vasco

/**
 * @author avbelyaev
 */
// hiding constructor UTE(String) and calling it from UTE(Object)
class UnexpectedTypeException private(msg: String) extends IllegalArgumentException(msg) {

  def this(obj: Object) = this(s"Wrong type ${obj.getClass} of ${obj.toString}")
}
