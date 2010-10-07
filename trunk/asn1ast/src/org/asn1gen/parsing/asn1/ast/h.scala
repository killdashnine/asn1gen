package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class HString(
  chars: String
) extends Node
  with OctetStringValue
  with BitStringValue {
}

