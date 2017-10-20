import scala.util.Try

case class Interpreter(text: String) {

  var pos: Int = 0
  var currentToken: Token = _
  var currentChar = text(pos)

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
        if (currentChar.isDigit) {
          return getInteger()
        } else if (currentChar == '+') {
          pos += 1
          return Token("Plus", currentChar.toString)
        } else if (currentChar == '-') {
          pos += 1
          return Token("Minus", currentChar.toString)
        } else
        error()
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
    currentToken = getNextToken()

    val left = currentToken
    eat("Integer")

    val plus = currentToken
    if (plus.typ == "Plus")
      eat("Plus")
    else
      eat("Minus")

    val right = currentToken
    eat("Integer")


    left.value.toInt + right.value.toInt
  }
}
