object Main extends App {

  while (true) {
    val line = scala.io.StdIn.readLine()
    val result =
      try {
        val tokens = Lexer(line)
        Interpreter.run(Parser.parseTokens(tokens))
      } catch {
        case e: Exception =>
          "Parsing failed: " + e.getClass.getName + " :: " + e.getMessage + "\n" + e.getStackTrace.mkString("\n")
      }
    println(result)
  }
}
