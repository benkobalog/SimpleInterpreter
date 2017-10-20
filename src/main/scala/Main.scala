object Main extends App {

  while (true) {
    val line = scala.io.StdIn.readLine()
    val interpreter = Interpreter(line)
    val result =
      try { interpreter.expr() } catch {
        case e => e.getMessage
      }
    println(result)
  }

}
