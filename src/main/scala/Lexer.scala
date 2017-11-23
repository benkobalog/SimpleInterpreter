case class Token(typ: String, value: String) {
  def toInt: Int = value.toInt
}

object TokenTypes {
  val Op = "Op"
  val Integer = "Integer"
  val EOF = "EOF"
  val Brackets = "Brackets"
}

class Lexer(val text: String) {
  import TokenTypes._
  var pos: Int = 0
  var currentToken: Token = _
  var currentChar: Char = text(pos)

  def error() = throw new Exception("Error parsing input")

  def getInteger: Token = {
    val numbers = text.substring(pos).takeWhile(_.isDigit)
    pos += numbers.length
    Token(Integer, numbers)
  }

  def getNextToken: Token = {
    while (pos < text.length) {
      currentChar = text(pos)
      if (!currentChar.isWhitespace) {
        return currentChar match {
          case x if x.isDigit =>
            getInteger
          case '+' | '-' | '*' | '/' =>
            pos += 1
            Token(Op, currentChar.toString)
          case '(' | ')' =>
            pos += 1
            Token(Brackets, currentChar.toString)
          case _ => error()
        }
      } else {
        pos += 1
      }
    }
    Token(EOF, "")
  }
}

object Lexer {
  import TokenTypes._
  def apply(text: String): Iterator[Token] = {
    val tokeniser = new Lexer(text)
    Stream
      .continually(tokeniser.getNextToken)
      .takeWhile(_.typ != EOF)
      .toIterator
  }
}
