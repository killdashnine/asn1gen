package org.asn1gen.gen.scala

import java.io.PrintWriter
import org.asn1gen.parsing.asn1.ast._
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

  def generate(assignmentList: AssignmentList): Unit = {
    assignmentList match {
      case AssignmentList(assignments) => assignments foreach { assignment =>
        generate(assignment)
      }
    }
  }

  def generate(assignment: Assignment): Unit = {
    assignment match {
      case TypeAssignment(TypeReference(name), _type: Type) => {
        generate(_type , name)
      }
      case va@ValueAssignment(valueReference, _type, value) => {
        out.println("/*")
        out.println(va)
        out.println("*/")
      }
    }
  }
  
  def generate(_type: Type, name: String): Unit = {
    _type match {
      case Type(builtinType: BuiltinType, _) => {
        generate(builtinType, name)
      }
      case t@Type(_, _) => {
        out.println("/*")
        out.println(t)
        out.println("*/")
      }
    }
  }
  
  def generate(builtinType: BuiltinType, assignmentName: String): Unit = {
    builtinType match {
      case ChoiceType(
        AlternativeTypeLists(rootAlternativeTypeList, _, _, _))
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
      case SequenceType(spec) => {
        out.print("case class " + safeId(assignmentName) + "(")
        spec match {
          case ComponentTypeLists(list1, extension, list2) => {
            out.println()
            out.indent(2) {
              list1 match {
                case Some(ComponentTypeList(list)) => {
                  generateSequenceConstructor(assignmentName, list)
                }
                case None => ()
              }
              out.println()
            }
          }
          case Empty => {}
        }
        out.println(") extends _runtime.AsnSequence {")
        out.indent(2) {
          spec match {
            case ComponentTypeLists(list1, extension, list2) => {
              list1 match {
                case Some(ComponentTypeList(list)) => {
                  generateSequenceImmutableSetters(assignmentName, list)
                }
                case None => ()
              }
            }
            case Empty => {}
          }
        }
        out.println("}")
        out.println()
        out.print("object " + safeId(assignmentName) + " extends " + safeId(assignmentName) + "(")
        out.indent(2) {
          spec match {
            case ComponentTypeLists(list1, extension, list2) => {
              out.println()
              list1 match {
                case Some(ComponentTypeList(list)) => {
                  var firstItem = true
                  list.map {
                    case NamedComponentType(
                      NamedType(_, _type),
                      optionalDefault)
                    => {
                      if (!firstItem) {
                        out.println(",")
                      }
                      optionalDefault match {
                        case Empty => {
                          out.print(safeId(typeNameOf(_type)))
                        }
                        case Optional => {
                          out.print("Some(" + safeId(typeNameOf(_type)) + ")")
                        }
                        case Default(value) => {
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
            case Empty => {}
          }
        }
        out.println(") {")
        out.println("}")
      }
      case EnumeratedType(enumerations)
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
  
  def generate(assignmentName: String, enumerations: Enumerations): Unit = {
    enumerations match {
      case Enumerations(RootEnumeration(Enumeration(items)), extension)
      => {
        var index = 0
        items foreach {
          case Identifier(item) => {
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
  
  def typeNameOf(namedComponentType: NamedComponentType): String = {
    namedComponentType match {
      case NamedComponentType(
        NamedType(Identifier(identifier), _type),
        value)
      => {
        typeNameOf(_type, value)
      }
    }
  }
  
  def typeNameOf(_type: Type, value: OptionalDefault[Value]): String = {
    value match {
      case Empty =>
        return typeNameOf(_type)
      case Default(value) =>
        return typeNameOf(_type)
      case Optional =>
        return "Option[" + typeNameOf(_type) + "]"
    }
  }
  
  def generateSequenceConstructor(
      sequenceName: String, list: List[ComponentType]): Unit = {
    var firstTime = true
    list foreach {
      case NamedComponentType(
        NamedType(Identifier(identifier), _type),
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
  
  def typeNameOf(_type: Type): String = {
    _type match {
      case Type(typeKind, _) => typeNameOf(typeKind)
    }
  }
  
  def typeNameOf(typeKind: TypeKind): String = {
    typeKind match {
      case builtinType: BuiltinType => typeNameOf(builtinType)
      case TypeReference(reference) => reference
      case unmatched => "Unmatched(" + unmatched + ")"
    }
  }
  
  def typeNameOf(typeKind: TypeKind, value: OptionalDefault[Value]): String = {
    value match {
      case Empty =>
        return typeNameOf(typeKind)
      case Default(value) =>
        return typeNameOf(typeKind)
      case Optional =>
        return "Option[" + typeNameOf(typeKind) + "]"
    }
  }
  
  def typeNameOf(builtinType: BuiltinType): String = {
    builtinType match {
      case BitStringType(_) => {
        return "_runtime.AsnBitString"
      }
      case BOOLEAN => {
        return "_runtime.AsnBoolean"
      }
      case characterString: CharacterStringType => {
        typeNameOf(characterString)
      }
      case _: ChoiceType => {
        return "_runtime.AsnChoice"
      }
      case EmbeddedPdvType => {
        return "_runtime.AsnEmbeddedPdv"
      }
      case EnumeratedType(_) => {
        return "_runtime.AsnEnumerated"
      }
      case EXTERNAL => {
        return "ExternalType"
      }
      case InstanceOfType(_) => {
        return "InstanceOfType"
      }
      case IntegerType(_) => {
        return "_runtime.AsnInteger"
      }
      case NULL => {
        return "_runtime.AsnNull"
      }
      case _: ObjectClassFieldType => {
        return "_runtime.AsnObjectClassField"
      }
      case ObjectIdentifierType => {
        return "_runtime.AsnObjectIdentifier"
      }
      case OctetStringType => {
        return "_runtime.AsnOctetString"
      }
      case REAL => {
        return "_runtime.AsnReal"
      }
      case RelativeOidType => {
        return "_runtime.AsnRelativeOidType"
      }
      case SequenceOfType(_) => {
        return "_runtime.AsnSequenceOf"
      }
      case SequenceType(_) => {
        return "_runtime.AsnSequence"
      }
      case SetOfType(_) => {
        return "_runtime.AsnSetOf"
      }
      case SetType(_) => {
        return "_runtime.AsnSet"
      }
      case TaggedType(_, _, underlyingType) => {
        return typeNameOf(underlyingType)
      }
      case unmatched => {
        return "UnknownBuiltinType(" + unmatched + ")"
      }
    }
  }
  
  def typeNameOf(characterString: CharacterStringType): String = {
    characterString match {
      case BMPString => {
        return "_runtime.AsnBmpString"
      }
      case GeneralString => {
        return "_runtime.AsnGeneralString"
      }
      case GraphicString => {
        return "_runtime.AsnGraphicString"
      }
      case IA5String => {
        return "_runtime.AsnIa5String"
      }
      case ISO646String => {
        return "_runtime.AsnIso646String"
      }
      case NumericString => {
        return "_runtime.AsnNumericString"
      }
      case PrintableString => {
        return "_runtime.AsnPrintableString"
      }
      case T61String => {
        return "_runtime.AsnT61String"
      }
      case TeletexString => {
        return "_runtime.AsnTeletexString"
      }
      case UniversalString => {
        return "_runtime.AsnUniversalString"
      }
      case UTF8String => {
        return "_runtime.AsnUtf8String"
      }
      case VideotexString => {
        return "_runtime.AsnVideotexString"
      }
      case VisibleString => {
        return "_runtime.AsnVisibleString"
      }
      case unknown => {
        return "UnknownCharacterString(" + unknown + ")"
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
        NamedType(Identifier(identifier), _type),
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
      _type: Type,
      value: OptionalDefault[Value],
      fieldNames: List[String]): Unit = {
    _type match {
      case Type(TaggedType(_, _, fieldType), _) => {
        generateSequenceImmutableSetter(sequenceName, fieldName, fieldType, value, fieldNames)
        //out.println("// tag " + number)
      }
      case Type(builtinType: TypeKind, List()) => {
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
      rootAlternativeTypeList: RootAlternativeTypeList): Unit = {
    rootAlternativeTypeList match {
      case RootAlternativeTypeList(AlternativeTypeList(namedTypes)) => {
        namedTypes foreach { namedType =>
          generateChoices(assignmentName, namedType)
        }
      }
    }
  }
  
  def generateChoices(
      assignmentName: String,
      namedType: NamedType): Unit = {
    namedType match {
      case NamedType(
        Identifier(name),
        Type(
          TaggedType(
            Tag(_, Number(tagNumber)), _, _type),
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
      rootAlternativeTypeList: RootAlternativeTypeList): Unit = {
    rootAlternativeTypeList match {
      case RootAlternativeTypeList(AlternativeTypeList(namedTypes)) => {
        namedTypes foreach { namedType =>
          generateChoiceFieldTransformer(choiceTypeName, namedType)
        }
      }
    }
  }

  def generateChoiceFieldTransformer(choiceTypeName: String, namedType: NamedType): Unit = {
    namedType match {
      case NamedType(
        Identifier(name),
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
