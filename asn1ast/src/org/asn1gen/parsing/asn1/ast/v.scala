package org.asn1gen.parsing.asn1.ast

trait Value
  extends ActualParameter
  with GovernorConstraintParameterValue
  with LowerEndValue
  with Setting
  with UpperEndValue {
}

case class ValueAssignment(
  valueReference: ValueReference,
  _type: Type,
  value: Value
) extends Node with Assignment {
  def name = valueReference.name
}

trait ValueConstraint {
}

case class ValueFieldReference(
  chars: String
) extends Node with PrimitiveFieldNameKind {
  def name = chars
}

case class ValueFromObject(
  referencedObjects: ReferencedObjects,
  fieldName: FieldName
) extends Node with ReferencedValue {
}

case class ValueOptionalitySpec(
    value: OptionalDefault[Value]
) extends Node {
}

case class ValueRange(
  lowerEndPoint: LowerEndPoint,
  upperEndPoint: UpperEndPoint
) extends Node with SubtypeElementsKind {
}

case class ValueReference (
  name: String
) extends Node
    with Reference
    with DefinedValue
    with SimpleDefinedValue {
}

case class ValueSet(
  elementSetSpecs: ElementSetSpecs
) extends Node
  with ActualParameter
  with GovernorConstraintParameterValue
  with Setting {
}

case class ValueSetFieldReference(
  chars: String
) extends Node with PrimitiveFieldNameKind {
  def name = chars
}

case class ValueSetFromObjects(
  referencedObjects: ReferencedObjects,
  fieldName: FieldName
) extends Node with ReferencedType {
}

case class ValueSetOptionalitySpec(
    value: OptionalDefault[ValueSet]
) extends Node {
}

case class ValueSetTypeAssignment(
  typeReference: TypeReference,
  _type: Type,
  valueSet: ValueSet
) extends Node with Assignment {
}

case class VariableTypeValueFieldSpec(
  valueFieldReference: ValueFieldReference,
  fieldName: FieldName,
  valueOptionalitySpec: ValueOptionalitySpec
) extends Node with FieldSpec {
}

case class VariableTypeValueSetFieldSpec(
  valueSetFieldReference: ValueSetFieldReference,
  fieldName: FieldName,
  valueSetOptionalitySpec: ValueSetOptionalitySpec
) extends Node with FieldSpec {
}

object VideotexString extends RestrictedCharacterStringType {
}

object VisibleString extends RestrictedCharacterStringType {
}
