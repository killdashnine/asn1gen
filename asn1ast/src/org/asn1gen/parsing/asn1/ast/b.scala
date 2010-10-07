package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

object BMPString extends RestrictedCharacterStringType {
}

object BOOLEAN extends BuiltinType {
}

case class BString(
  chars: String
) extends Node
  with OctetStringValue
  with BitStringValue {
}

case class BasicComponentType(
  _type: Type
) extends ComponentType {
}


case class BitStringType(
  namedBits: Option[List[NamedBit]]
) extends Node with BuiltinType {
}

trait BitStringValue extends BuiltinValue {
}

case class BooleanValue(
  value: Boolean
) extends Node with BuiltinValue {
}

trait BuiltinType extends TypeKind {
}


trait BuiltinValue extends Node
  with ObjectClassFieldValueKind
  with FixedTypeFieldValKind
  with Value {
}
