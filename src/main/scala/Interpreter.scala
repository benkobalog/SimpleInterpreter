object Interpreter {

  def run(ast: AST): Int = {
    ast match {
      case BinOp(op, left, right) =>
        val lRes = run(left)
        val rRes = run(right)
        op match {
          case "+" => lRes + rRes
          case "-" => lRes - rRes
          case "*" => lRes * rRes
          case "/" => lRes / rRes
        }
      case Num(x) => x
    }
  }
}
