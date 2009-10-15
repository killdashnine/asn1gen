package org.asn1gen.parsing.asn1

class AstNode {}

case class AstLowerId(name: String) extends AstNode {
}

case class AstUpperId(name: String) extends AstNode {
}

case class AstPairId(lt : AstLowerId, rt : AstUpperId) extends AstNode {
}

