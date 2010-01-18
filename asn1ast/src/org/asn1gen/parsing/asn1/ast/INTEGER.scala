package org.asn1gen.parsing.asn1.ast

case class INTEGER(
  namedNumbers: Option[List[NamedNumber]]
) extends Node with BuiltinType {
}
