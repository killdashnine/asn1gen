package org.asn1gen.parsing.asn1.ast

case class HString(
  chars: String
) extends Node
  with OctetStringValue
  with BitStringValue {
}

