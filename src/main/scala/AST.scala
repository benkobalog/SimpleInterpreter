sealed trait AST
case class BinOp(op: String, left: AST, right: AST) extends AST
case class Num(value: Int) extends AST
