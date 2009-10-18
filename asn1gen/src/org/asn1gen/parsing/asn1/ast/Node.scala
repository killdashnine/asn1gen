package org.asn1gen.parsing.asn1.ast

class Node {
}

case class ModuleDefinition(reference : ModuleReference) extends Node {
  def name = reference.name
}

case class BString(chars : String) extends Node {
}

case class CString(chars : String) extends Node {
}

case class ModuleReference(chars : String) extends Node {
  def name = chars
}

