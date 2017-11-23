import org.scalatest._

class InterpreterSpec extends FlatSpec with Matchers {
  "An arithmetic interpreter" should "return correct results" in {
    testInterpreter(s => Interpreter.run(Parser.parseTokens(Lexer(s))))
  }

  private def testInterpreter(interpreterFN: String => Int) = {
    assert(interpreterFN("1+2") == 3)
    assert(interpreterFN("3") == 3)
    assert(interpreterFN("2 + 7 * 4") == 30)
    assert(interpreterFN("7 + 3 * (10 / (12 / (3 + 1) - 1))") == 22)
    assert(interpreterFN(
      "7 + 3 * (10 / (12 / (3 + 1) - 1)) / (2 + 3) - 5 - 3 + (8)") == 10)
    assert(interpreterFN("7 + (((3 + 2)))") == 12)
  }

  "Interpreter" should "return correct results" in {
    val ast = BinOp("+", Num(3), BinOp("*", Num(8), Num(6)))
    assert(Interpreter.run(ast) == 51)
    val ast2 = BinOp("-", Num(123), Num(23))
    assert(Interpreter.run(ast2) == 100)
  }
}
