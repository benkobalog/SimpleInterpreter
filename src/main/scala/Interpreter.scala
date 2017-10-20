import scala.util.Try

case class Interpreter(text: String) {

  var pos: Int = 0
  var currentToken: Token = _
  var currentChar: Char = text(pos)

  def error() = throw new Exception("Error parsing input")

  def getInteger(): Token = {
    val numbers = text.substring(pos).takeWhile(_.isDigit)
    pos += numbers.length
    Token("Integer", numbers)
  }

  def getNextToken(): Token = {
    while (pos < text.length) {
      currentChar = text(pos)
      if (!currentChar.isWhitespace) {
        return currentChar match {
          case x if x.isDigit =>
            getInteger()
          case '+' | '-' | '*' | '/' =>
            pos += 1
            Token("Op", currentChar.toString)
          case _ => error()
        }
      } else {
        pos += 1
      }
    }
    Token("EOF", "")
  }

  def eat(typ: String): Unit =
    if (currentToken.typ == typ)
      currentToken = getNextToken()
    else {
      println(currentToken)
      error()
    }

  def expr(): Int = {
    val tokens =
      Stream.continually(getNextToken()).takeWhile(_.typ != "EOF").toList

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
