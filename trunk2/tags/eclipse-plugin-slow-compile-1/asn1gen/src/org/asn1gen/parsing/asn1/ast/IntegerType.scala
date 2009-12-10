package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class IntegerType(
  namedNumbers: Option[List[NamedNumber]]
) extends Node with BuiltinType {
  def typeName: String = "AsnInteger"
}
