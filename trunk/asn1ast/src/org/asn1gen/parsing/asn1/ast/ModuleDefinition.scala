package org.asn1gen.parsing.asn1.ast

case class ModuleDefinition(
  identifier: ModuleIdentifier,
  tagDefault: TagDefault,
  extensionDefault: ExtensionDefault,
  moduleBody: ModuleBody
) extends Node {
  def name = identifier.name
  def exports = moduleBody.exports
  def imports = moduleBody.imports
  def assignmentList = moduleBody.assignmentList
}
