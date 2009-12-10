package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class BitStringType(
  namedBits: Option[List[NamedBit]]
) extends Node with BuiltinType {
  def typeName: String = "AsnBitString"
}
