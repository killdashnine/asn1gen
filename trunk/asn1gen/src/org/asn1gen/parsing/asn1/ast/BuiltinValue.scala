package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

trait BuiltinValue extends Node
  with ObjectClassFieldValueKind
  with FixedTypeFieldValKind
  with Value {
}
