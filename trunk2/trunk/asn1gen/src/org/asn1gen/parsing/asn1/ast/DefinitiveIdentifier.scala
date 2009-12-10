package org.asn1gen.parsing.asn1.ast

case class DefinitiveIdentifier(
  definitiveObjectIdComponents: Option[List[DefinitiveObjectIdComponent]]
) extends Node {
}
