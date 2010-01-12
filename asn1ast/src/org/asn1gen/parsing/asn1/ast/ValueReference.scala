package org.asn1gen.parsing.asn1.ast

case class ValueReference (
  name: String
) extends Node
    with Reference
    with DefinedValue
    with SimpleDefinedValue {
}
