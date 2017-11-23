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

      case UnaryOp(op, expr) =>
        op match {
          case "+" => run(expr)
          case "-" => -run(expr)
        }

      case Num(x) => x
    }
  }
}
