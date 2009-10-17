package org.asn1gen.parsing.asn1.ast

class Node {}

case class ModuleDefinition(name : ModuleReference2) extends Node {
}

case class ModuleReference2(name : String) extends Node {
}

