package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

trait ReferencedValue
  extends ObjectClassFieldValueKind
  with FixedTypeFieldValKind
  with Value {
}
