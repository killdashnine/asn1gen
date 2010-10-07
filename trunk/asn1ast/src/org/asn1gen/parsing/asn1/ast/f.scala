package org.asn1gen.parsing.asn1.ast

object FALSE extends BooleanValue(false) {
}

case class FieldName(
  primitiveFieldNames: List[PrimitiveFieldName]
) extends Node {
}

case class FieldSetting(
    primitiveFieldName: PrimitiveFieldName,
    setting: Setting
) extends Node {
}

trait FieldSpec {
}

case class FixedTypeFieldVal(
  kind: FixedTypeFieldValKind
) extends Node with ObjectClassFieldValueKind {
}

trait FixedTypeFieldValKind {
}

case class FixedTypeValueFieldSpec(
  valueFieldReference: ValueFieldReference,
  _type: Type,
  unique: Option[UNIQUE],
  valueOptionalitySpec: ValueOptionalitySpec
) extends Node with FieldSpec{
}

case class FixedTypeValueSetFieldSpec(
  valueSetFieldReference: ValueSetFieldReference,
  _type: Type,
  valueSetOptionalitySpec: ValueSetOptionalitySpec
) extends Node with FieldSpec {
}

case class FullSpecification(
  typeConstraints: TypeConstraints
) extends Node with MultipleTypeConstraints {
}
