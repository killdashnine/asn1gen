package org.asn1gen.gen

import java.io.PrintWriter
import org.asn1gen.parsing.asn1.ast._
import org.asn1gen.io._

class GenScala(out: IndentWriter) {
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
        out.println("package " + moduleName +" {")
        out.indent(2) {
          out.println("import org.asn1gen.runtime._")
          out.println()
          generate(assignmentList)
        }
        out.println("}")
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
        out.println("case class " + name + "(override val choice: AsnType) extends AsnChoice(choice) {")
        out.indent(2) {
          out.println("//////////////////////////////////////////////////////////////////")
          out.println("// Choice IDs")
          generateChoiceIds(rootAlternativeTypeList)
          generateSimpleGetters(rootAlternativeTypeList)
        }
        out.println("}")
      }
      case SequenceType(ComponentTypeLists(list1, extension, list2))
      => {
        out.println("case class " + name + "(")
        out.indent(2) {
          list1 match {
            case Some(ComponentTypeList(list)) => {
              generateSequenceConstructor(name, list)
            }
            case None => ()
          }
        }
        out.println()
        out.println(") extends AsnSequence {")
        out.indent(2) {
          list1 match {
            case Some(ComponentTypeList(list)) => {
              generateSequenceImmutableSetters(name, list)
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
    var firstTime = true
    list foreach {
      case NamedComponentType(
        NamedType(Identifier(identifier), componentType),
        value)
      => {
        if (!firstTime) {
          out.println(",")
        }
        out.print(identifier + ": " + typeNameOf(componentType))
        firstTime = false
      }
    }
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
  
  def generateSequenceImmutableSetters(sequenceName: String, list: List[ComponentType]): Unit = {
    val fieldNames = list.map {
      case NamedComponentType(
        NamedType(Identifier(identifier), _),
        _)
      => identifier
    }
    list foreach {
      case NamedComponentType(
        NamedType(Identifier(identifier), componentType),
        value)
      => {
        generateSequenceImmutableSetter(sequenceName: String, identifier, componentType, fieldNames)
      }
    }
  }
  
  def generateSequenceImmutableSetter(
      sequenceName: String,
      fieldName: String,
      type_ : Type_,
      fieldNames: List[String]): Unit = {
    type_ match {
      case Type_(TaggedType(_, _, fieldType), _) => {
        generateSequenceImmutableSetter(sequenceName, fieldName, fieldType, fieldNames)
        //out.println("// tag " + number)
      }
      case Type_(IntegerType(None), List()) => {
        out.println(
            "def " + fieldName + "(f: (" + "AsnInteger" + " => " +
            "AsnInteger" + ")): " + sequenceName + " = " + sequenceName + "(")
        var firstIteration = true
        out.indent(2) {
          fieldNames foreach { listedFieldName =>
            if (!firstIteration) {
              out.println(",")
            }
            if (listedFieldName == fieldName) {
              out.print("f(this." + fieldName + ")")
            } else {
              out.print("this." + listedFieldName)
            }
            firstIteration = false
          }
          out.println(")")
        }
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
        out.println("val " + name.toUpperCase + " = " + tagNumber + ": Integer")
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
        out.println()
        out.println("def " + name + ": AsnInteger = choice_.asInstanceOf[AsnInteger]")
      }
    }
  }
}
