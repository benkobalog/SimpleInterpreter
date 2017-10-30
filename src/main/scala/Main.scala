object Main extends App {

  while (true) {
    val line = scala.io.StdIn.readLine()
    val interpreter = Interpreter(line)
    val result =
      try {
        interpreter.process(Tokens(line).toIterator)
      } catch {
        case e: Exception =>
          "Parsing failed: " + e.getClass.getName + " :: " + e.getMessage + "\n" + e.getStackTrace.mkString("\n")
      }
    println(result)
  }
}
