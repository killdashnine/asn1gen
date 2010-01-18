package org.asn1gen.gen

import java.io.PrintWriter
import org.asn1gen.parsing.asn1.ast._
import org.asn1gen.io._

class GenJava(out: IndentWriter) {
  
  def generate(moduleDefinition: ModuleDefinition): Unit = {
    moduleDefinition match {
      case moduleDefinition@ModuleDefinition(
        ModuleIdentifier(
          ModuleReference(moduleName),
          DefinitiveIdentifier(_)),
        _,
        ExtensionDefault(_),
        ModuleBody(_, _, assignmentList))
      => {
        out.println("package " + moduleName +";")
        out.println()
        generate(assignmentList)
        out.println()
      }
    }
  }

  def generate(assignmentList: AssignmentList): Unit = {
    assignmentList match {
      case AssignmentList(assignments) => assignments foreach { assignment =>
        generate(assignment)
      }
    }
  }

  def generate(assignment: Assignment): Unit = {
    assignment match {
      case TypeAssignment(
        TypeReference(name),
        _type : Type)
      => {
        generate(_type , name)
      }
    }
  }
  
  def generate(_type: Type, name: String): Unit = {
    _type match {
      case Type(builtinType: BuiltinType, _) => {
        generate(builtinType, name)
      }
    }
  }
  
  def generate(builtinType: BuiltinType, name: String): Unit = {
    builtinType match {
      case ChoiceType(
        AlternativeTypeLists(rootAlternativeTypeList, _, _, _))
      => {
        out.println("public class " + name + " {")
        out.indent(2) {
          out.println("//////////////////////////////////////////////////////////////////")
          out.println("// Choice IDs")
          generateChoiceIds(rootAlternativeTypeList)
          out.println("private AsnType elem_;")
          out.println()
          generateSimpleGetters(rootAlternativeTypeList)
        }
        out.println("}")
      }
      case SequenceType(ComponentTypeLists(list1, extension, list2))
      => {
        out.println("public class " + name + " extends AsnSequence {")
        out.indent(2) {
          out.println()
          list1 match {
            case Some(ComponentTypeList(list)) => {
              generateSequenceConstructor(name, list)
              generateSequenceFields(list)
            }
            case None => ()
          }
        }
        out.println("}")
      }
    }
  }
  
  def generateSequenceConstructor(
      sequenceName: String, list: List[ComponentType]): Unit = {
    out.println("public " + sequenceName + "(")
    out.indent(2) {
      var firstTime = true
      list foreach {
        case NamedComponentType(
          NamedType(Identifier(identifier), componentType),
          value)
        => {
          if (!firstTime) {
            out.println(",")
          }
          out.print("final " + typeNameOf(componentType) + " " + identifier)
          firstTime = false
        }
      }
    }
    out.println(")")
    out.println("{")
    out.indent(2) {
      list foreach {
        case NamedComponentType(
          NamedType(Identifier(identifier), componentType),
          value)
        => {
          out.println("this." + identifier + " = " + identifier + ";")
        }
      }
    }
    out.println("}")
    out.println()
  }
  
  def typeNameOf(_type: Type): String = {
    _type match {
      case Type(typeKind, _) => typeNameOf(typeKind)
    }
  }
  
  def typeNameOf(typeKind: TypeKind): String = {
    typeKind match {
      case builtinType: BuiltinType => typeNameOf(builtinType)
    }
  }
  
  def typeNameOf(builtinType: BuiltinType): String = {
    builtinType match {
      case BitStringType(_) => {
        return "AsnBitString"
      }
      case BOOLEAN => {
        return "AsnBoolean"
      }
      case _: CharacterStringType => {
        return "AsnCharacterString"
      }
      case _: ChoiceType => {
        return "AsnChoice"
      }
      case EmbeddedPdvType => {
        return "AdnEmbeddedPdv"
      }
      case EnumeratedType(_) => {
        return "AsnEnumerated"
      }
      case EXTERNAL => {
        return "ExternalType"
      }
      case InstanceOfType(_) => {
        return "InstanceOfType"
      }
      case INTEGER(_) => {
        return "AsnInteger"
      }
      case NULL => {
        return "AsnNull"
      }
      case _: ObjectClassFieldType => {
        return "AsnObjectClassField"
      }
      case ObjectIdentifierType => {
        return "AsnObjectIdentifier"
      }
      case OctetStringType => {
        return "AsnOctetString"
      }
      case REAL => {
        return "AsnReal"
      }
      case RelativeOidType => {
        return "AsnRelativeOidType"
      }
      case SequenceOfType(_) => {
        return "AsnSequenceOf"
      }
      case SequenceType(_) => {
        return "AsnSequence"
      }
      case SetOfType(_) => {
        return "AsnSetOf"
      }
      case SetType(_) => {
        return "AsnSet"
      }
      case TaggedType(_, _, underlyingType) => {
        return typeNameOf(underlyingType)
      }
      case unmatched => {
        return "UnknownBuiltinType(" + unmatched + ")"
      }
    }
  }
  
  def generateSequenceFields(list: List[ComponentType]): Unit = {
    list foreach {
      case NamedComponentType(
        NamedType(Identifier(identifier), componentType),
        value)
      => {
        generateSequenceField(identifier, componentType)
      }
    }
  }
  
  def generateSequenceField(identifier: String, _type: Type): Unit = {
    _type match {
      case Type(TaggedType(_, _, underlyingType), _) => {
        generateSequenceField(identifier, underlyingType)
        //out.println("// tag " + number)
      }
      case Type(INTEGER(None), List()) => {
        out.println("public final " + typeNameOf(_type) + " " + identifier + ";");
      }
      case unmatched => {
        out.println("// Unmatched type: " + unmatched)
      }
    }
  }
  
  def generateChoiceIds(rootAlternativeTypeList: RootAlternativeTypeList): Unit = {
    rootAlternativeTypeList match {
      case RootAlternativeTypeList(AlternativeTypeList(namedTypes)) => {
        namedTypes foreach { namedType =>
          generateChoiceIds(namedType)
        }
      }
    }
  }
  
  def generateChoiceIds(namedType: NamedType): Unit = {
    namedType match {
      case NamedType(
        Identifier(name),
        Type(
          TaggedType(
            Tag(_, Number(tagNumber)), _, _),
          _))
      => {
        out.println()
        out.println("@ChoiceId")
        out.println("public static int " + name.toUpperCase + " = " + tagNumber + ";")
      }
    }
  }

  def generateSimpleGetters(rootAlternativeTypeList: RootAlternativeTypeList): Unit = {
    rootAlternativeTypeList match {
      case RootAlternativeTypeList(AlternativeTypeList(namedTypes)) => {
        namedTypes foreach { namedType =>
          generateSimpleGetters(namedType)
        }
      }
    }
  }
  
  def generateSimpleGetters(namedType: NamedType): Unit = {
    namedType match {
      case NamedType(
        Identifier(name),
        Type(
          TaggedType(_, _, _type),
          _))
      => {
        val getter = "get" + name.head.toUpper + name.substring(1)
        out.println()
        out.println("public " + typeNameOf(_type) + " " + getter + "() {")
        out.indent(2) {
          out.println("return (AsnInteger)elem_;")
        }
        out.println("}")
      }
    }
  }
}
