package org.asn1gen.parsing.asn1.ast

case class Value() extends Node {
}

case class ValueAssignment() extends Node {
}

case class ValueConstraint() extends Node {
}

case class ValueFieldReference(chars : String) extends Node {
  def name = chars
}

case class ValueFromObject() extends Node {
}

case class ValueOptionalitySpec() extends Node {
}

case class ValueRange() extends Node {
}

case class ValueReference(chars : String) extends Node {
  def name = chars
}

case class ValueSet() extends Node {
}

case class ValueSetFieldReference(chars : String) extends Node {
  def name = chars
}

case class ValueSetFromObjects() extends Node {
}

case class ValueSetOptionalitySpec() extends Node {
}

case class ValueSetTypeAssignment() extends Node {
}

case class VariableTypeValueFieldSpec() extends Node {
}

case class VariableTypeValueSetFieldSpec() extends Node {
}

case class Word(chars : String) extends Node {
  def name = chars
}

case class WithSyntaxSpec() extends Node {
}
