package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class TaggedType(
  tag: Tag,
  taggedKind: TaggedKind,
  _type: Type
) extends Node with BuiltinType {
}
