package org.asn1gen.gen.java

import org.asn1gen.parsing.asn1.{ast => ast}

case class NamedType(name: String, _type: ast.Type) {
}

object NamedType {
  def from(typeAssignment: ast.TypeAssignment): NamedType =
    NamedType(typeAssignment.name, typeAssignment._type)
}
