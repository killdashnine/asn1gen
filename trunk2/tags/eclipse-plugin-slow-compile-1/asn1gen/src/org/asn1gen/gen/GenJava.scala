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
        type_ : Type_)
      => {
        generate(type_ , name)
      }
    }
  }
  
  def generate(type_ : Type_, name: String): Unit = {
    type_ match {
      case Type_(builtinType: BuiltinType, _) => {
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
  
  def typeNameOf(type_ : Type_): String = {
    type_ match {
      case Type_(TaggedType(_, _, underlyingType), _) => {
        return typeNameOf(underlyingType)
      }
      case Type_(IntegerType(_), _) => {
        return "AsnInteger"
      }
      case unmatched => {
        return "Unknown(" + unmatched + ")"
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
  
  def generateSequenceField(identifier: String, type_ : Type_): Unit = {
    type_ match {
      case Type_(TaggedType(_, _, underlyingType), _) => {
        generateSequenceField(identifier, underlyingType)
        //out.println("// tag " + number)
      }
      case Type_(IntegerType(None), List()) => {
        out.println("public final AsnInteger " + identifier + ";");
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
        Type_(
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
        Type_(
          TaggedType(_, _, type_),
          _))
      => {
        val getter = "get" + name.first.toUpperCase + name.substring(1)
        out.println()
        out.println("public AsnInteger " + getter + "() {")
        out.indent(2) {
          out.println("return (AsnInteger)elem_;")
        }
        out.println("}")
      }
    }
  }
}
