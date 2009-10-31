package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ParameterizedTypeAssignment(
  typeReference: TypeReference,
  parameterList: ParameterList,
  type_ : Type_ 
) extends Node with ParameterizedAssignmentKind {
}
