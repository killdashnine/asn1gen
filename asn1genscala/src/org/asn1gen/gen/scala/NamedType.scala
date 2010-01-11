package org.asn1gen.gen.scala

import org.asn1gen.parsing.asn1.{ast => ast}

case class NamedType(name: String) {
}

object NamedType {
  def from(typeAssignment: ast.TypeAssignment): NamedType = NamedType(typeAssignment.name.name)
}
