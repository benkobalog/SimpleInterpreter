import scala.util.Try

case class Interpreter(text: String) {
  import TokenTypes._

  def error() = throw new Exception("Parsing error")

  var currentToken: Token = _

  def process(tokens: Iterator[Token]): Int = {

    def getNextToken(): Token = {
      if (tokens.hasNext) {
        tokens.next()
      } else {
        Token(EOF, "")
      }
    }

    def eat(tokenType: String): Unit = {
      if (currentToken.typ == tokenType)
        currentToken = getNextToken()
      else
        error()
    }

    def factor(): Int = {
      val token = currentToken
      eat(Integer)
      token.toInt
    }

    def term(): Int = {
      println(s"term $currentToken")

      var result = factor()

      while (List("*", "/").contains(currentToken.value)) {
        println(s"term inner: $currentToken")
        val token = currentToken
        if (token.value == "*") {
          eat(Op)
          result *= factor()
        } else if (token.value == "/") {
          eat(Op)
          result /= factor()
        }
      }

      result
    }

    def expr(): Int = {
      println(s"expr $currentToken")
      var result = term()

      while (List("+", "-").contains(currentToken.value)) {
        val token = currentToken
        if (token.typ == Op) {
          if (token.value == "+") {
            eat(Op)
            result = result + term()
          } else if (token.value == "-") {
            eat(Op)
            result = result - term()
          }
        }
      }

      result
    }

    currentToken = tokens.next()
    expr()
  }

}
