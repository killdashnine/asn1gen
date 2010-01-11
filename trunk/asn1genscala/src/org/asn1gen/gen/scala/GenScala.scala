package org.asn1gen.gen.scala

import java.io.PrintWriter
import org.asn1gen.parsing.asn1.{ast => ast}
import org.asn1gen.io._
import scala.collection.immutable.Set

class GenScala(out: IndentWriter) {
  val keywords = Set("yield", "type", "null")
  
  def safeId(id: String): String = {
    if (keywords contains id) {
      return "`" + id + "`"
    } else {
      return id.replaceAll("-", "_")
    }
  }
  
  var moduleName: Option[String] = None
  
  def generate(moduleDefinition: ast.ModuleDefinition): Unit = {
    moduleDefinition match {
      case moduleDefinition@ast.ModuleDefinition(
        ast.ModuleIdentifier(
          ast.ModuleReference(moduleName),
          ast.DefinitiveIdentifier(_)),
        _,
        ast.ExtensionDefault(_),
        ast.ModuleBody(_, _, assignmentList))
      => {
        out.println("package " + this.moduleName.getOrElse(moduleName) +" {")
        out.indent(2) {
          out.println("import org.asn1gen.{runtime => _runtime}")
          out.println()
          generate(assignmentList)
        }
        out.println("}")
      }
    }
  }

  def generate(assignmentList: ast.AssignmentList): Unit = {
    assignmentList match {
      case ast.AssignmentList(assignments) => assignments foreach { assignment =>
        generate(assignment)
      }
    }
  }

  def generate(assignment: ast.Assignment): Unit = {
    assignment match {
      case ast.TypeAssignment(ast.TypeReference(name), _type: ast.Type) => {
        generate(_type , name)
      }
      case va@ast.ValueAssignment(valueReference, _type, value) => {
        out.println("/*")
        out.println(va)
        out.println("*/")
      }
    }
  }
  
  def generate(_type: ast.Type, name: String): Unit = {
    _type match {
      case ast.Type(builtinType: ast.BuiltinType, _) => {
        generate(builtinType, name)
      }
      case t@ast.Type(_, _) => {
        out.println("/*")
        out.println(t)
        out.println("*/")
      }
    }
  }
  
  def generate(builtinType: ast.BuiltinType, assignmentName: String): Unit = {
    builtinType match {
      case ast.ChoiceType(
        ast.AlternativeTypeLists(rootAlternativeTypeList, _, _, _))
      => {
        out.println(
            "abstract class " + safeId(assignmentName) +
            "(_element: _runtime.AsnType) extends _runtime.AsnChoice {")
        out.indent(2) {
          out.println("def _choice: Int")
          generateSimpleGetters(rootAlternativeTypeList)
          generateChoiceFieldTransformers(assignmentName, rootAlternativeTypeList)
        }
        out.println("}")
        generateChoices(assignmentName, rootAlternativeTypeList)
        val firstNamedType =
          rootAlternativeTypeList.alternativeTypeList.namedTypes(0)
        out.println()
        out.println(
            "object " + safeId(assignmentName) + " extends " +
            safeId(assignmentName + "_" + firstNamedType.id.name) +
            "(" + typeNameOf(firstNamedType._type) + ") {")
        out.println("}")
      }
      case ast.SequenceType(spec) => {
        out.print("case class " + safeId(assignmentName) + "(")
        spec match {
          case ast.ComponentTypeLists(list1, extension, list2) => {
            out.println()
            out.indent(2) {
              list1 match {
                case Some(ast.ComponentTypeList(list)) => {
                  generateSequenceConstructor(assignmentName, list)
                }
                case None => ()
              }
              out.println()
            }
          }
          case ast.Empty => {}
        }
        out.println(") extends _runtime.AsnSequence {")
        out.indent(2) {
          spec match {
            case ast.ComponentTypeLists(list1, extension, list2) => {
              list1 match {
                case Some(ast.ComponentTypeList(list)) => {
                  generateSequenceImmutableSetters(assignmentName, list)
                }
                case None => ()
              }
            }
            case ast.Empty => {}
          }
        }
        out.println("}")
        out.println()
        out.print("object " + safeId(assignmentName) + " extends " + safeId(assignmentName) + "(")
        out.indent(2) {
          spec match {
            case ast.ComponentTypeLists(list1, extension, list2) => {
              out.println()
              list1 match {
                case Some(ast.ComponentTypeList(list)) => {
                  var firstItem = true
                  list.map {
                    case ast.NamedComponentType(
                      ast.NamedType(_, _type),
                      optionalDefault)
                    => {
                      if (!firstItem) {
                        out.println(",")
                      }
                      optionalDefault match {
                        case ast.Empty => {
                          out.print(safeId(typeNameOf(_type)))
                        }
                        case ast.Optional => {
                          out.print("Some(" + safeId(typeNameOf(_type)) + ")")
                        }
                        case ast.Default(value) => {
                          out.print("/* Default(" + value + ") */")
                        }
                      }
                      firstItem = false
                    }
                  }
                  out.println()
                }
                case None => ()
              }
            }
            case ast.Empty => {}
          }
        }
        out.println(") {")
        out.println("}")
      }
      case ast.EnumeratedType(enumerations)
      => {
        out.println("case class " + safeId(assignmentName) + "(_value: Int) extends _runtime.AsnEnumeration {")
        out.println("}")
        out.println()
        out.println("object " + safeId(assignmentName) + " extends " + safeId(assignmentName) + "(0) {")
        out.indent(2) {
          generate(assignmentName, enumerations)
        }
        out.println("}")
      }
      case unmatched => {
        out.println("// Unmatched: " + unmatched)
      }
    }
  }
  
  def generate(assignmentName: String, enumerations: ast.Enumerations): Unit = {
    enumerations match {
      case ast.Enumerations(ast.RootEnumeration(ast.Enumeration(items)), extension)
      => {
        var index = 0
        items foreach {
          case ast.Identifier(item) => {
            out.println(
              "def " + safeId(item) + ": " + safeId(assignmentName) +
              " = " + safeId(assignmentName) + "(" + index + ")")
            index = index + 1
          }
          case v@_ => {
            out.println("/*")
            out.println(v)
            out.println("*/")
          }
        }
        extension match {
          case None => {}
          case _ => out.println(extension)
        }
      }
    }
  }
  
  def typeNameOf(namedComponentType: ast.NamedComponentType): String = {
    namedComponentType match {
      case ast.NamedComponentType(
        ast.NamedType(ast.Identifier(identifier), _type),
        value)
      => {
        typeNameOf(_type, value)
      }
    }
  }
  
  def typeNameOf(_type: ast.Type, value: ast.OptionalDefault[ast.Value]): String = {
    value match {
      case ast.Empty =>
        return typeNameOf(_type)
      case ast.Default(value) =>
        return typeNameOf(_type)
      case ast.Optional =>
        return "Option[" + typeNameOf(_type) + "]"
    }
  }
  
  def generateSequenceConstructor(
      sequenceName: String, list: List[ast.ComponentType]): Unit = {
    var firstTime = true
    list foreach {
      case ast.NamedComponentType(
        ast.NamedType(ast.Identifier(identifier), _type),
        value)
      => {
        if (!firstTime) {
          out.println(",")
        }
        out.print(safeId(identifier) + ": " + safeId(typeNameOf(_type, value)))
        firstTime = false
      }
    }
  }
  
  def typeNameOf(_type: ast.Type): String = {
    _type match {
      case ast.Type(typeKind, _) => typeNameOf(typeKind)
    }
  }
  
  def typeNameOf(typeKind: ast.TypeKind): String = {
    typeKind match {
      case builtinType: ast.BuiltinType => typeNameOf(builtinType)
      case ast.TypeReference(reference) => reference
      case unmatched => "Unmatched(" + unmatched + ")"
    }
  }
  
  def typeNameOf(typeKind: ast.TypeKind, value: ast.OptionalDefault[ast.Value]): String = {
    value match {
      case ast.Empty =>
        return typeNameOf(typeKind)
      case ast.Default(value) =>
        return typeNameOf(typeKind)
      case ast.Optional =>
        return "Option[" + typeNameOf(typeKind) + "]"
    }
  }
  
  def typeNameOf(builtinType: ast.BuiltinType): String = {
    builtinType match {
      case ast.BitStringType(_) => {
        return "_runtime.AsnBitString"
      }
      case ast.BOOLEAN => {
        return "_runtime.AsnBoolean"
      }
      case characterString: ast.CharacterStringType => {
        typeNameOf(characterString)
      }
      case _: ast.ChoiceType => {
        return "_runtime.AsnChoice"
      }
      case ast.EmbeddedPdvType => {
        return "_runtime.AsnEmbeddedPdv"
      }
      case ast.EnumeratedType(_) => {
        return "_runtime.AsnEnumerated"
      }
      case ast.EXTERNAL => {
        return "ExternalType"
      }
      case ast.InstanceOfType(_) => {
        return "InstanceOfType"
      }
      case ast.IntegerType(_) => {
        return "_runtime.AsnInteger"
      }
      case ast.NULL => {
        return "_runtime.AsnNull"
      }
      case _: ast.ObjectClassFieldType => {
        return "_runtime.AsnObjectClassField"
      }
      case ast.ObjectIdentifierType => {
        return "_runtime.AsnObjectIdentifier"
      }
      case ast.OctetStringType => {
        return "_runtime.AsnOctetString"
      }
      case ast.REAL => {
        return "_runtime.AsnReal"
      }
      case ast.RelativeOidType => {
        return "_runtime.AsnRelativeOidType"
      }
      case ast.SequenceOfType(_) => {
        return "_runtime.AsnSequenceOf"
      }
      case ast.SequenceType(_) => {
        return "_runtime.AsnSequence"
      }
      case ast.SetOfType(_) => {
        return "_runtime.AsnSetOf"
      }
      case ast.SetType(_) => {
        return "_runtime.AsnSet"
      }
      case ast.TaggedType(_, _, underlyingType) => {
        return typeNameOf(underlyingType)
      }
      case unmatched => {
        return "UnknownBuiltinType(" + unmatched + ")"
      }
    }
  }
  
  def typeNameOf(characterString: ast.CharacterStringType): String = {
    characterString match {
      case ast.BMPString => {
        return "_runtime.AsnBmpString"
      }
      case ast.GeneralString => {
        return "_runtime.AsnGeneralString"
      }
      case ast.GraphicString => {
        return "_runtime.AsnGraphicString"
      }
      case ast.IA5String => {
        return "_runtime.AsnIa5String"
      }
      case ast.ISO646String => {
        return "_runtime.AsnIso646String"
      }
      case ast.NumericString => {
        return "_runtime.AsnNumericString"
      }
      case ast.PrintableString => {
        return "_runtime.AsnPrintableString"
      }
      case ast.T61String => {
        return "_runtime.AsnT61String"
      }
      case ast.TeletexString => {
        return "_runtime.AsnTeletexString"
      }
      case ast.UniversalString => {
        return "_runtime.AsnUniversalString"
      }
      case ast.UTF8String => {
        return "_runtime.AsnUtf8String"
      }
      case ast.VideotexString => {
        return "_runtime.AsnVideotexString"
      }
      case ast.VisibleString => {
        return "_runtime.AsnVisibleString"
      }
      case unknown => {
        return "UnknownCharacterString(" + unknown + ")"
      }
    }
  }
  
  def generateSequenceImmutableSetters(sequenceName: String, list: List[ast.ComponentType]): Unit = {
    val fieldNames = list.map {
      case ast.NamedComponentType(
        ast.NamedType(ast.Identifier(identifier), _),
        _)
      => identifier
    }
    list foreach {
      case ast.NamedComponentType(
        ast.NamedType(ast.Identifier(identifier), _type),
        value)
      => {
        generateSequenceImmutableSetter(
            sequenceName: String, identifier, _type, value, fieldNames)
      }
    }
  }
  
  def generateSequenceImmutableSetter(
      sequenceName: String,
      fieldName: String,
      _type: ast.Type,
      value: ast.OptionalDefault[ast.Value],
      fieldNames: List[String]): Unit = {
    _type match {
      case ast.Type(ast.TaggedType(_, _, fieldType), _) => {
        generateSequenceImmutableSetter(sequenceName, fieldName, fieldType, value, fieldNames)
        //out.println("// tag " + number)
      }
      case ast.Type(builtinType: ast.TypeKind, List()) => {
        val setterType = typeNameOf(builtinType, value)
        out.println(
            "def " + safeId(fieldName) + "(f: (" + setterType + " => " +
            setterType + ")): " + sequenceName + " = " + sequenceName + "(")
        var firstIteration = true
        out.indent(2) {
          fieldNames foreach { listedFieldName =>
            if (!firstIteration) {
              out.println(",")
            }
            if (listedFieldName == fieldName) {
              out.print("f(this." + safeId(fieldName) + ")")
            } else {
              out.print("this." + safeId(listedFieldName))
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
  
  def generateChoices(
      assignmentName: String,
      rootAlternativeTypeList: ast.RootAlternativeTypeList): Unit = {
    rootAlternativeTypeList match {
      case ast.RootAlternativeTypeList(ast.AlternativeTypeList(namedTypes)) => {
        namedTypes foreach { namedType =>
          generateChoices(assignmentName, namedType)
        }
      }
    }
  }
  
  def generateChoices(
      assignmentName: String,
      namedType: ast.NamedType): Unit = {
    namedType match {
      case ast.NamedType(
        ast.Identifier(name),
        ast.Type(
          ast.TaggedType(
            ast.Tag(_, ast.Number(tagNumber)), _, _type),
          _))
      => {
        out.println()
        out.println(
            "case class " + safeId(assignmentName + "_" + name) +
            "(_element: " + typeNameOf(_type) + ") extends MyChoice(_element) {")
        out.indent(2) {
          out.println("def _choice: Int = " + tagNumber)
        }
        out.println("}")
      }
    }
  }

  def generateChoiceFieldTransformers(
      choiceTypeName: String,
      rootAlternativeTypeList: ast.RootAlternativeTypeList): Unit = {
    rootAlternativeTypeList match {
      case ast.RootAlternativeTypeList(ast.AlternativeTypeList(namedTypes)) => {
        namedTypes foreach { namedType =>
          generateChoiceFieldTransformer(choiceTypeName, namedType)
        }
      }
    }
  }

  def generateChoiceFieldTransformer(choiceTypeName: String, namedType: ast.NamedType): Unit = {
    namedType match {
      case ast.NamedType(
        ast.Identifier(name),
        _type)
      => {
        out.println()
        out.println(
            "def " + safeId(name) +
            "(f: (" + safeId(choiceTypeName) + " => " + safeId(typeNameOf(_type)) +
            ")): " + safeId(choiceTypeName) + " =")
        out.indent(2) {
          out.println(
              choiceTypeName + "_" + name + "(f(this))")
        }
      }
    }
  }

  def generateSimpleGetters(rootAlternativeTypeList: ast.RootAlternativeTypeList): Unit = {
    rootAlternativeTypeList match {
      case ast.RootAlternativeTypeList(ast.AlternativeTypeList(namedTypes)) => {
        namedTypes foreach { namedType =>
          generateSimpleGetters(namedType)
        }
      }
    }
  }
  
  def generateSimpleGetters(namedType: ast.NamedType): Unit = {
    namedType match {
      case ast.NamedType(
        ast.Identifier(name),
        _type)
      => {
        out.println()
        out.println(
            "def " + safeId(name) + ": " + safeId(typeNameOf(_type)) +
            " = _element.asInstanceOf[" + typeNameOf(_type) + "]")
      }
    }
  }
}
