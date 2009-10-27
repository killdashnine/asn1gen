package org.asn1gen.parsing.asn1.ast

case class TaggedType(tag: Tag, type_ : Type_) extends Node with BuiltinTypeKind {
}
