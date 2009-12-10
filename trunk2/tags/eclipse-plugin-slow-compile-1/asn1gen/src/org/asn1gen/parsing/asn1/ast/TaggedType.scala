package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class TaggedType(
  tag: Tag,
  taggedKind: TaggedKind,
  type_ : Type_
) extends Node with BuiltinType {
  def typeName: String = "<TaggedType>"
}
