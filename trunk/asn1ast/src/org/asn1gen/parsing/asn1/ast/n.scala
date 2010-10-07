package org.asn1gen.parsing.asn1.ast

import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional

case class NameAndNumberForm(
  identifier: Identifier,
  numberForm: NumberForm
) extends Node with ObjIdComponents with RelativeOidComponentsKind {
}

case class NameForm(
  identifier: Identifier
) extends Node
  with DefinitiveObjectIdComponent
  with ObjIdComponents {
}

case class NamedBit(
  identifier: Identifier,
  kind: NamedBitKind
) extends Node {
}

trait NamedBitKind {
}

case class NamedComponentType(
  namedType: NamedType,
  value: OptionalDefault[Value]
) extends ComponentType {
  def name = namedType.name
}

case class NamedConstraint(
  identifier: Identifier,
  componentConstraint: ComponentConstraint
) extends Node {
}

case class NamedNumber(
  identifier: Identifier,
  value: NamedNumberValue
) extends Node with EnumerationItem {
  def name = identifier.name
}

trait NamedNumberValue {

}

case class NamedType(
  identifier: Identifier,
  _type: Type
) extends Node with ExtensionAdditionAlternative {
  def name = identifier.name
  def typeKind = _type.kind
  def constraints = _type.constraints
}

case class NamedValue(
  identifier: Identifier,
  value: Value
) extends Node {
}

class Node extends Positional {
  def withPosition(pos: Position): Node = {
    this.pos = pos
    return this
  }
}

case class NonZeroNumber() extends Node {
}

object NULL extends Node with BuiltinType with BuiltinValue {
}

case class Number(
  chars: String
) extends Node
    with NumberForm
    with ClassNumber
    with NumericRealValue
    with NamedBitKind {
  def negative = Number("-" + chars)
}

trait NumberForm
  extends ObjIdComponents
  with RelativeOidComponentsKind
{
}

trait NumericRealValue extends RealValue {
}

object NumericString extends RestrictedCharacterStringType {
}
