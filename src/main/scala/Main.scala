object Main extends App {

  while (true) {
    val line = scala.io.StdIn.readLine()
    val interpreter = Interpreter(line)
    val result =
      try {
        val tokens = Tokens(line)
        interpreter.process(tokens)
      } catch {
        case e: Exception =>
          "Parsing failed: " + e.getClass.getName + " :: " + e.getMessage + "\n" + e.getStackTrace.mkString("\n")
      }
    println(result)
  }
}
