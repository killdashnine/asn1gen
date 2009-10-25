package org.asn1gen.parsing.asn1.ast

case class ImplicitTaggedType(
  override val tag: Tag,
  override val type_ : Type
) extends TaggedType(tag, type_) {

}
