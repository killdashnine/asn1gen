package org.asn1gen.parsing.asn1.ast

case class ComponentTypeLists(
  list1: Option[RootComponentTypeList],
  extension: Option[ComponentTypeListsExtension],
  list2: Option[RootComponentTypeList]
) extends Node with SetTypeSpec with SequenceTypeSpec {
}
