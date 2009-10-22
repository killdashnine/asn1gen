package org.asn1gen.parsing.asn1.ast

case class GeneralConstraint() extends Node {
}

case class GlobalModuleReference() extends Node {
}

case class Governor() extends Node {
}

case class Group() extends Node {
}

case class HString(chars : String) extends Node {
}

case class Identifier(chars : String) extends Node {
  def name = chars
}

case class IElems() extends Node {
}

case class IdentifierList() extends Node {
}

case class Imports() extends Node {
}

case class Includes() extends Node {
}

case class InnerTypeConstraints() extends Node {
}

case class InstanceOfType() extends Node {
}

case class InstanceOfValue() extends Node {
}

case class IntegerType() extends Node {
}

case class IntegerValue() extends Node {
}

case class IntersectionElements() extends Node {
}

case class IntersectionMark() extends Node {
}

case class Intersections() extends Node {
}
