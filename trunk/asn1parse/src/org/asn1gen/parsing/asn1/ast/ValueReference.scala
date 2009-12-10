package org.asn1gen.parsing.asn1.ast

case class ValueReference (
  chars: String
) extends Node
    with Reference
    with DefinedValue
    with SimpleDefinedValue {
  def name = chars
}
