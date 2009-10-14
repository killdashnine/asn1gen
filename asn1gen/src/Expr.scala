sealed abstract class Expr {
    def eval():Double
}

case class EConst(value:Double) extends Expr {
    def eval():Double = value
}

case class EAdd(left:Expr, right:Expr) extends Expr {
	def eval():Double = left.eval + right.eval
}
