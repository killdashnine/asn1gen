package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.syntax.Asn1Tokens

trait Asn1Nodes {
  class Node {
    object Tokens extends Asn1Tokens {}
  }
  
  case class BString(chars : String) extends Node {
  }
  
  case class CString(chars : String) extends Node {
  }
  
  case class HString(chars : String) extends Node {
  }
  
  case class Identifier(chars : String) extends Node {
  }
  
  case class ModuleDefinition(reference : ModuleReference) extends Node {
    def name = reference.name
  }
  
  case class ModuleReference(chars : String) extends Node {
    def name = chars
  }
  
  case class Number(chars : String) extends Node {
    def negative = Number("-" + chars)
  }
  
  case class ObjectClassReference(chars : String) extends Node {
    def name = chars
  }
  
  case class ObjectFieldReference(chars : String) extends Node {
    def name = chars
  }
  
  case class ObjectSetFieldReference(chars : String) extends Node {
    def name = chars
  }
  
  case class ObjectSetReference(chars : String) extends Node {
    def name = chars
  }
  
  case class TypeFieldReference(chars : String) extends Node {
    def name = chars
  }
  
  case class TypeReference(chars : String) extends Node {
    def name = chars
    def asModuleReference = ModuleReference(chars)
  }

  case class ValueFieldReference(chars : String) extends Node {
    def name = chars
  }

  case class ValueReference(chars : String) extends Node {
    def name = chars
  }

  case class ValueSetFieldReference(chars : String) extends Node {
    def name = chars
  }
}
