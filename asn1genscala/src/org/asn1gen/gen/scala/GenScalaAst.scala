package org.asn1gen.gen.scala

import org.asn1gen.parsing.asn1.ast._
import org.asn1gen.parsing.asn1.{ast => ast}
import org.asn1gen.parsing.asn1.ast.kind._
import org.asn1gen.io._

object GenScalaAst {
  def generate(out: IndentWriter, moduleDefinition: ModuleDefinition): Unit = {
    moduleDefinition match {
      case ModuleDefinition(
        moduleIdentifier: ModuleIdentifier,
        tagDefault: TagDefault,
        extensionDefault: ExtensionDefault,
        moduleBody: ModuleBody) => {
          out.println("ModuleDefinition(")
          out.indent(2) {
            generate(out, moduleIdentifier)
            out.println(",")
            generate(out, tagDefault)
            out.println(",")
            generate(out, extensionDefault)
            out.println(",")
            generate(out, moduleBody)
            out.print(")")
          }
        }
    }
  }
  
  def generate(out: IndentWriter, moduleIdentifier: ModuleIdentifier): Unit = {
    out.print("?moduleIdentifier")
  }
  
  def generate(out: IndentWriter, tagDefault: TagDefault): Unit = {
    out.print("?tagDefault")
  }
  
  def generate(out: IndentWriter, extensionDefault: ExtensionDefault): Unit = {
    out.println("?extensionDefault")
  }
  
  def generate(out: IndentWriter, moduleBody: ModuleBody): Unit = {
    moduleBody match {
      case ModuleBody(
        exports: Exports,
        imports: Imports,
        assignmentList: AssignmentList) => {
          out.println("ModuleBody(")
          out.indent(2) {
            generate(out, exports)
            out.println(",")
            generate(out, imports)
            out.println(",")
            generate(out, assignmentList)
            out.print(")")
          }
        }
    }
  }
  
  def generate(out: IndentWriter, exports: Exports): Unit = {
    out.print("?exports")
  }
  
  def generate(out: IndentWriter, imports: Imports): Unit = {
    out.print("?imports")
  }
  
  def generate(out: IndentWriter, assignmentList: AssignmentList): Unit = {
    assignmentList match {
      case AssignmentList(assignments: List[Assignment]) => {
          out.print("AssignmentList(")
          generate(out, assignments) { assignment =>
            generate(out, assignment)
          }
          out.print(")")
        }
    }
  }
  
  def generate(out: IndentWriter, assignment: Assignment): Unit = {
    assignment match {
      case TypeAssignment(
        typeReference: TypeReference,
        _type: Type) => {
          out.println("TypeAssignment(")
          out.indent(2) {
            generate(out, typeReference)
            out.println(",")
            generate(out, _type)
            out.print(")")
          }
        }
      case ValueAssignment(
        valueReference: ValueReference,
        _type: Type,
        value: Value) => {
          out.println("ValueAssignment(")
          out.indent(2) {
            generate(out, valueReference)
            out.println(",")
            generate(out, _type)
            out.println(",")
            generate(out, value)
            out.print(")")
          }
        }
    }
  }
  
  def generate(out: IndentWriter, typeReference: TypeReference): Unit = {
    typeReference match {
      case TypeReference(name) => {
        out.print("TypeReference(\"" + name + "\")")
      }
    }
  }
  
  def generate(out: IndentWriter, _type: Type): Unit = {
    _type match {
      case Type(kind: TypeKind, constraints: List[Constraint]) => {
        out.println("Type(")
        out.indent(2) {
          generate(out, kind)
          out.println(",")
          generate(out, constraints) { constraint =>
            generate(out, constraint)
          }
          out.print(")")
        }
      }
    }
  }
  
  def generate(out: IndentWriter, typeKind: TypeKind): Unit = {
    typeKind match {
      // BuiltinType
      case BitStringType(maybeNamedBits: Option[List[NamedBit]]) => {
        generate(out, maybeNamedBits) { namedBits =>
          out.println("?namedBits")
        }
      }
      case BOOLEAN => {
        out.println("BOOLEAN")
      }
      case characterStringType: CharacterStringType => {
        characterStringType match {
          case restrictedCharacterStringType: RestrictedCharacterStringType => {
            restrictedCharacterStringType match {
              case BMPString => out.print("BMPString")
              case GeneralString => out.print("GeneralString")
              case IA5String => out.print("IA5String")
              case NumericString => out.print("NumericString")
              case TeletexString => out.print("TeletexString")
              case UniversalString => out.print("UniversalString")
              case VideotexString => out.print("VideotexString")
              case GraphicString => out.print("GraphicString")
              case ISO646String => out.print("ISO646String")
              case PrintableString => out.print("PrintableString")
              case T61String => out.print("T61String")
              case UTF8String => out.print("UTF8String")
              case VisibleString => out.print("VisibleString")
            }
          }
          case UnrestrictedCharacterStringType => {
            out.print("CHARACTER STRING")
          }
        }
      }
      case ChoiceType(typeLists: AlternativeTypeLists) => {
        out.println("ChoiceType(")
        out.indent(2) {
          generate(out, typeLists)
          out.print(")")
        }
      }
      case EmbeddedPdvType => out.println("EmbeddedPdvType")
      case EnumeratedType(enumerations: Enumerations) => {
        out.println("EnumeratedType(")
        out.indent(2) {
          generate(out, enumerations)
          out.print(")")
        }
      }
      case EXTERNAL => out.print("EXTERNAL")
      case InstanceOfType(definedObjectClass: DefinedObjectClass) => {
        out.println("InstanceOfType(")
        out.indent(2) {
          generate(out, definedObjectClass)
          out.print(")")
        }
      }
      case INTEGER(maybeNamedNumbers: Option[List[NamedNumber]]) => {
        out.println("INTEGER(")
        out.indent(2) {
          generate(out, maybeNamedNumbers) { namedNumbers =>
            generate(out, namedNumbers) { namedNumber =>
              generate(out, namedNumber)
            }
          }
          out.print(")")
        }
      }
      case NULL => out.print("NULL")
      case ObjectClassFieldType(definedObjectClass: DefinedObjectClass, fieldName: FieldName) => {
        out.println("ObjectClassFieldType(")
        out.indent(2) {
          generate(out, definedObjectClass)
          out.println(",")
          generate(out, fieldName)
          out.print(")")
        }
      }
      case ObjectIdentifierType => out.print("ObjectIdentifierType")
      case OctetStringType => out.print("OctetStringType")
      case REAL => out.print("REAL")
      case RelativeOidType => out.print("RelativeOidType")
      case SequenceOfType(_type: Type) => {
        out.println("SequenceOfType(")
        out.indent(2) {
          generate(out, _type)
          out.print(")")
        }
      }
      case SequenceType(sequenceTypeSpec: SequenceTypeSpec) => {
        out.println("SequenceType(")
        out.indent(2) {
          generate(out, sequenceTypeSpec)
          out.print(")")
        }
      }
      case SetOfType(_type: Type) => {
        out.println("SetOfType(")
        out.indent(2) {
          generate(out, _type)
          out.print(")")
        }
      }
      case SetType(spec: SetTypeSpec) => {
        out.println("SetType(")
        out.indent(2) {
          generate(out, spec)
          out.print(")")
        }
      }
      case TaggedType(tag: Tag, taggedKind: TaggedKind, _type: Type) => {
        out.println("TaggedType(")
        out.indent(2) {
          generate(out, tag)
          out.println(",")
          generate(out, taggedKind)
          out.println(",")
          generate(out, _type)
          out.print(")")
        }
      }
      // ConstrainedType
      case ConstrainedType(typeWithConstraint: TypeWithConstraint) => {
        out.println("ConstrainedType(")
        out.indent(2) {
          generate(out, typeWithConstraint)
          out.print(")")
        }
      }
      // ReferencedType
      case definedType: DefinedType => {
        definedType match {
          case ParameterizedType(
            simpleDefinedType: SimpleDefinedType,
            actualParameterList: ActualParameterList) => {
              out.println("ParameterizedType(")
              out.indent(2) {
                generate(out, simpleDefinedType)
                out.println(",")
                generate(out, actualParameterList)
                out.print(")")
              }
            }
          case ParameterizedValueSetType(
            simpleDefinedType: SimpleDefinedType,
            actualParameterList: ActualParameterList) => {
              out.println("ParameterizedValueSetType(")
              out.indent(2) {
                generate(out, simpleDefinedType)
                out.println(",")
                generate(out, actualParameterList)
                out.print(")")
              }
            }
          case ExternalTypeReference(
            moduleReference: ModuleReference,
            typeReference: TypeReference) => {
              out.println("ParameterizedValueSetType(")
              out.indent(2) {
                generate(out, moduleReference)
                out.println(",")
                generate(out, typeReference)
                out.print(")")
              }
            }
          case TypeReference(name: String) => {
            out.print("TypeReference(\"" + name + "\")")
          }
        }
      }
      case usefulType: UsefulType => {
        usefulType match {
          case GeneralizedTime => out.print("GeneralizedTime")
          case UTCTime => out.print("UTCTime")
          case ObjectDescriptor => out.print("ObjectDescriptor")
        }
      }
      case SelectionType(identifier: Identifier, _type: Type) => {
        out.println("SelectionType(")
        out.indent(2) {
          generate(out, identifier)
          out.println(",")
          generate(out, _type)
          out.print(")")
        }
      }
      case TypeFromObject(referencedObjects: ReferencedObjects, fieldName: FieldName) => {
        out.println("TypeFromObject(")
        out.indent(2) {
          generate(out, referencedObjects)
          out.println(",")
          generate(out, fieldName)
          out.print(")")
        }
      }
      case ValueSetFromObjects(referencedObjects: ReferencedObjects, fieldName: FieldName) => {
        out.println("ValueSetFromObjects(")
        out.indent(2) {
          generate(out, referencedObjects)
          out.println(",")
          generate(out, fieldName)
          out.print(")")
        }
      }
    }
  }
  
  def generate(out: IndentWriter, typeLists: AlternativeTypeLists): Unit = {
    typeLists match {
      case AlternativeTypeLists(
        typeList: RootAlternativeTypeList,
        maybeExtensionAndException: Option[ExtensionAndException],
        maybeExtensionAdditionAlternatives: Option[ExtensionAdditionAlternatives],
        maybeOptionalExtensionMarker: Option[OptionalExtensionMarker]) => {
          out.println("AlternativeTypeLists(")
          out.indent(2) {
            generate(out, typeList)
            out.println(",")
            generate(out, maybeExtensionAndException) { extensionAndException =>
              generate(out, extensionAndException)
            }
            out.println(",")
            generate(out, maybeExtensionAdditionAlternatives) { extensionAdditionAlternatives =>
              generate(out, extensionAdditionAlternatives)
            }
            out.println(",")
            generate(out, maybeOptionalExtensionMarker) { optionalExtensionMarker =>
              generate(out, optionalExtensionMarker)
            }
            out.print(")")
          }
        }
    }
  }
  
  def generate(out: IndentWriter, rootAlternativeTypeList: RootAlternativeTypeList): Unit = {
    out.print("?RootAlternativeTypeList")
  }
  
  def generate(out: IndentWriter, extensionAndException: ExtensionAndException): Unit = {
    out.print("?ExtensionAndException")
  }
  
  def generate(
      out: IndentWriter,
      extensionAdditionAlternatives: ExtensionAdditionAlternatives): Unit = {
    out.print("?ExtensionAdditionAlternatives")
  }
  
  def generate(out: IndentWriter, optionalExtensionMarker: OptionalExtensionMarker): Unit = {
    out.print("?OptionalExtensionMarker")
  }
  
  def generate(out: IndentWriter, enumerations: Enumerations): Unit = {
    out.print("?Enumerations")
  }
  
  def generate(out: IndentWriter, definedObjectClass: DefinedObjectClass): Unit = {
    out.print("?DefinedObjectClass")
  }
  
  def generate(out: IndentWriter, namedNumber: NamedNumber): Unit = {
    out.print("?NamedNumber")
  }
  
  def generate(out: IndentWriter, fieldName: FieldName): Unit = {
    out.print("?FieldName")
  }
  
  def generate(out: IndentWriter, sequenceTypeSpec: SequenceTypeSpec): Unit = {
    sequenceTypeSpec match {
      case extensionSequenceTypeSpec: ExtensionSequenceTypeSpec => {
        generate(out, extensionSequenceTypeSpec)
      }
      case componentTypeLists: ComponentTypeLists => {
        generate(out, componentTypeLists)
      }
      case empty: Empty => generate(out, empty)
    }
  }
  
  def generate(out: IndentWriter, componentTypeLists: ComponentTypeLists): Unit = {
    componentTypeLists match {
      case ComponentTypeLists(
        maybeList1: Option[ComponentTypeList],
        maybeExtension: Option[ComponentTypeListsExtension],
        maybeList2: Option[ComponentTypeList]) => {
          out.println("ComponentTypeLists(")
          out.indent(2) {
            generate(out, maybeList1) { list =>
              generate(out, list)
            }
            out.println(",")
            generate(out, maybeExtension) { extension =>
              generate(out, extension)
            }
            out.println(",")
            generate(out, maybeList2) { list =>
              generate(out, list)
            }
            out.print(")")
          }
        }
    }
  }
  
  def generate(out: IndentWriter, componentTypeList: ComponentTypeList): Unit = {
    out.println("ComponentTypeList(")
    out.indent(2) {
      componentTypeList match {
        case ComponentTypeList(componentTypes: List[ComponentType]) => {
          generate(out, componentTypes) { componentType =>
            generate(out, componentType)
          }
        }
      }
      out.print(")")
    }
  }
  
  def generate(out: IndentWriter, componentType: ComponentType): Unit = {
    componentType match {
      case namedComponentType: NamedComponentType => {
        generate(out, namedComponentType)
      }
      case basicComponentType: BasicComponentType => {
        generate(out, basicComponentType)
      }
    }
  }
  
  def generate(out: IndentWriter, namedComponentType: NamedComponentType): Unit = {
    namedComponentType match {
      case NamedComponentType(
        namedType: ast.NamedType,
        maybeValue: OptionalDefault[Value]) => {
          out.println("NamedComponentType(")
          out.indent(2) {
            generate(out, namedType)
            out.println(",")
            generate(out, maybeValue) { value =>
              generate(out, value)
            }
            out.print(")")
          }
        }
    }
  }
  
  def generate(out: IndentWriter, namedType: ast.NamedType): Unit = {
    namedType match {
      case ast.NamedType(identifier: Identifier, _type: Type) => {
        out.println("NamedType(")
        out.indent(2) {
          generate(out, identifier)
          out.println(",")
          generate(out, _type)
          out.print(")")
        }
      }
    }
  }
  
  def generate(out: IndentWriter, identifier: Identifier): Unit = {
    identifier match {
      case Identifier(identifier) => {
        out.print("Identifier(\"" + identifier + "\")")
      }
    }
  }
  
  def generate[T](out: IndentWriter, optionalDefault: OptionalDefault[T])(f: T => Unit): Unit = {
    optionalDefault match {
      case Empty => generate(out, Empty)
      case Optional => out.print("Optional")
      case Default(value) => {
        out.println("Default(")
        out.indent(2) {
          f(value)
          out.print(")")
        }
      }
    }
  }
  
  def generate(out: IndentWriter, basicComponentType: BasicComponentType): Unit = {
    out.print("?BasicComponentType")
  }
  
  def generate(out: IndentWriter, componentTypeListsExtension: ComponentTypeListsExtension): Unit = {
    out.print("?ComponentTypeListsExtension")
  }
  
  def generate(out: IndentWriter, empty: Empty): Unit = {
    out.print("Empty")
  }
  
  def generate(out: IndentWriter, setTypeSpec: SetTypeSpec): Unit = {
    out.print("?SetTypeSpec")
  }
  
  def generate(out: IndentWriter, Tag: Tag): Unit = {
    out.print("?Tag")
  }
  
  def generate(out: IndentWriter, taggedKind: TaggedKind): Unit = {
    out.print("?TaggedKind")
  }
  
  def generate(out: IndentWriter, typeWithConstraint: TypeWithConstraint): Unit = {
    out.print("?TypeWithConstraint")
  }
  
  def generate(out: IndentWriter, simpleDefinedType: SimpleDefinedType): Unit = {
    out.print("?simpleDefinedType")
  }
  
  def generate(out: IndentWriter, actualParameterList: ActualParameterList): Unit = {
    out.print("?ActualParameterList")
  }
  
  def generate(out: IndentWriter, moduleReference: ModuleReference): Unit = {
    out.print("?ModuleReference")
  }
  
  def generate(out: IndentWriter, referencedObjects: ReferencedObjects): Unit = {
    out.print("?ReferencedObjects")
  }
  
  def generate(out: IndentWriter, constraint: Constraint): Unit = {
    out.print("?Constraint")
  }
  
  def generate(out: IndentWriter, valueReference: ValueReference): Unit = {
    valueReference match {
      case ValueReference(name) => {
        out.print("ValueReference(\"" + name + "\"")
      }
    }
  }
  
  def generate(out: IndentWriter, value: Value): Unit = {
    out.print("?Value")
  }
  
  def generate[T](out: IndentWriter, maybe: Option[T])(f: T => Unit): Unit = {
    maybe match {
      case None => out.print("None")
      case Some(v) => {
        out.print("Some(")
        f(v)
        out.print(")")
      }
    }
  }
  
  def generate[T](out: IndentWriter, list: List[T])(f: T => Unit): Unit = {
    out.print("List(")
    if (list.size != 0) {
      out.println()
    }
    out.indent(2) {
      var firstTime = true
      list foreach { element =>
        if (!firstTime) {
          out.println(",")
        }
        f(element)
        firstTime = false
      }
      out.print(")")
    }
  }
}
