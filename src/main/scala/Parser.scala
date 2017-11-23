import TokenTypes.{Brackets, EOF, Integer, Op}

object Parser {

  var currentToken: Token = _
  import TokenTypes._
  def error() = throw new Exception("Parsing error")

  def parseTokens(tokens: Iterator[Token]): AST = {

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

    def factor(): AST = {
      val token = currentToken
      if (token.typ == Brackets) {
        eat(Brackets)
        val node = expr()
        eat(Brackets)
        node
      } else if (token.typ == Integer) {
        eat(Integer)
        Num(token.toInt)
      } else {
        error()
      }
    }

    def term(): AST = {

      var node = factor()

      while (List("*", "/").contains(currentToken.value)) {
        val token = currentToken
        if (token.value == "*") {
          eat(Op)
        } else if (token.value == "/") {
          eat(Op)
        }
        node = BinOp(token.value, node, factor())
      }

      node
    }

    def expr(): AST = {
      var node = term()

      while (List("+", "-").contains(currentToken.value)) {
        val token = currentToken
        if (token.typ == Op) {
          if (token.value == "+") {
            eat(Op)
          } else if (token.value == "-") {
            eat(Op)
          }
          node = BinOp(token.value, node, term())
        }
      }

      node
    }

    currentToken = tokens.next()
    expr()
  }

}
