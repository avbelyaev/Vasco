package ru.avbelyaev.vasco

/**
 * @author avbelyaev
 */
object VascoCompiler {

//  private val PROGRAM_TEXT =
//    """(define sq
//      |   (lambda (x) (* x x)))
//      |""".stripMargin.strip

  // TODO eval text into tokens via toolbox
  // private val PROGRAM_TOKENS =
  //    List("define", "sq",
  //      List("lambda", List("x"), List("*", "x", "x")))
  private val PROGRAM_TOKENS = List("lambda", List("x"), List("+", "x", "a"))

  def main(args: Array[String]): Unit = {
    val tokensExp = Expression.of(PROGRAM_TOKENS)
    println(s"Tokens:\n$tokensExp")

    val closureConvertedTokens = ClosureConverter.convertClosure(PROGRAM_TOKENS)
    println(closureConvertedTokens)
  }

}
