package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ComponentTypeLists(
  list1: Option[RootComponentTypeList],
  extension: Option[ComponentTypeListsExtension],
  list2: Option[RootComponentTypeList]
) extends Node with SetTypeKind with SequenceTypeSpec {
}
