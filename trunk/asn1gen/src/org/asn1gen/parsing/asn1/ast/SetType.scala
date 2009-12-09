package org.asn1gen.parsing.asn1.ast

case class SetType(
  spec: SetTypeSpec
) extends BuiltinType {
  def typeName: String = "<SetType>"
}
