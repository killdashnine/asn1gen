package org.asn1gen.gen

import java.io._
import org.asn1gen.parsing.asn1.ast._

object GenJavaTags {
  def generate(moduleDefinition: ModuleDefinition, out: PrintWriter): Unit = {
    moduleDefinition match {
      case ModuleDefinition(
            ModuleIdentifier(
              ModuleReference("ModuleName"),
              DefinitiveIdentifier()),
            TagDefault(),
            ExtensionDefault(),
            ModuleBody(_, _, assignmentList))
      => {
        out.println("package mypackage;")
        out.println()
      }
    }
  }
  
  def generate(assignmentList: AssignmentList, out: PrintWriter): Unit = {
    assignmentList match {
      case AssignmentList(list) => {
        list foreach { assignment: Assignment =>
          assignment match {
            case Assignment(
              TypeAssignment(
                name,
                Type(BuiltinType(ChoiceType(alternativeTypeLists)))))
            => {
              out.println("public class " + name + "{")
              generate(alternativeTypeLists, out)
              out.println("}")
            }
          }
        }
      }
    }
  }
  
  def generate(alternativeTypeLists: AlternativeTypeLists, out: PrintWriter): Unit = {
    alternativeTypeLists match {
      case AlternativeTypeLists(
        RootAlternativeTypeList(namedTypes), _, _, _)
      => {
        namedTypes foreach { namedType : NamedType =>
          generate(namedType, out)
        }
      }
    }
  }
  
  def generate(namedType: NamedType, out: PrintWriter): Unit = {
    namedType match {
      case NamedType(id, Type(BuiltinType(TaggedType(Tag(_, number), _)))) =>
        out.println("/* " + id + " " + number + "*/")
    }
  }
}
