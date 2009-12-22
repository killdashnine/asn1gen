package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class OpenTypeFieldVal(
  _type: Type,
  value: Value
) extends Node with ObjectClassFieldValueKind {
}
