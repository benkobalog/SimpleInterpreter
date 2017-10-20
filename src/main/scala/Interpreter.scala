import scala.util.Try

case class Interpreter(text: String) {

  def expr(): Int = {
    val tokeniser = new Tokeniser(text)
    val tokens =
      Stream.continually(tokeniser.getNextToken).takeWhile(_.typ != "EOF").toList

    tokens.sliding(3, 2).foldLeft(tokens.head.value.toInt) {
      case (sum, List(_, op, x)) =>
        val int = x.value.toInt
        op.value match {
          case "+" => sum + int
          case "-" => sum - int
          case "*" => sum * int
          case "/" => sum / int
        }
    }
  }
}
