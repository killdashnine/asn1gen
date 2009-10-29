package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ParameterizedType(
  simpleDefinedType: SimpleDefinedType,
  actualParameterList: ActualParameterList
) extends Node with DefinedTypeKind {
}

