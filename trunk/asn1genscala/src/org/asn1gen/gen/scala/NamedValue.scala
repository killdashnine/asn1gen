package org.asn1gen.gen.scala

import org.asn1gen.parsing.asn1.{ast => ast}

case class NamedValue(name: String, _type: ast.Type, value: ast.Value) {
}

object NamedValue {
  def from(valueAssignment: ast.ValueAssignment): NamedValue =
    NamedValue(valueAssignment.name, valueAssignment._type, valueAssignment.value)
}
