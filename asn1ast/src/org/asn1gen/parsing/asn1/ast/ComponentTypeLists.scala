package org.asn1gen.parsing.asn1.ast

case class ComponentTypeLists(
  list1: Option[ComponentTypeList],
  extension: Option[ComponentTypeListsExtension],
  list2: Option[ComponentTypeList]
) extends Node with SetTypeSpec with SequenceTypeSpec {
}
