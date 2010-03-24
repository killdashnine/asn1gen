package org.asn1gen.gen.scala

import org.asn1gen.parsing.asn1.ast._
import org.asn1gen.parsing.asn1.{ ast => ast }

object AnonymousTypeNamer {
  def process(moduleDefinition: ModuleDefinition): ModuleDefinition =
    moduleDefinition.copy(moduleBody = process(moduleDefinition.moduleBody))
  
  def process(moduleBody: ModuleBody): ModuleBody =
    moduleBody.copy(assignmentList = process(moduleBody.assignmentList))
  
  def process(assignmentList: AssignmentList): AssignmentList =
    assignmentList.copy(assignments = process(assignmentList.assignments))
  
  def process(assignments: List[Assignment]): List[Assignment] =
    assignments.flatMap(process)
  
  def process(assignment: Assignment): List[Assignment] = assignment match {
    case typeAssignment: TypeAssignment => process(typeAssignment)
    case valueAssignment: ValueAssignment => process(valueAssignment)
  }
  
  def process(typeAssignment: TypeAssignment): List[Assignment] = {
    typeAssignment match {
      case TypeAssignment(typeReference: TypeReference, _type: Type) => {
        val redirectedType = redirectMembers(typeReference.name, _type)
        val redirectedAssignment = TypeAssignment(typeReference, redirectedType)
        val decoupledAssignments = decoupleMembers(typeReference.name, _type)
        redirectedAssignment::decoupledAssignments
      }
    }
  }
  
  def process(valueAssignment: ValueAssignment): List[Assignment] =
    valueAssignment::decoupleMembers(valueAssignment.name, valueAssignment._type)
  
  /**
   * @return (replacementType, newAssignments)
   */
  def decoupleNamedType(parentName: String, name: String, _type: Type): List[Assignment] = {
    _type match {
      case Type(typeKind, constraints) => {
        typeKind match {
          case TaggedType(tag: Tag, taggedKind, subType: Type) => {
            decoupleNamedType(parentName, name, subType)
          }
          case typeReference: TypeReference => {
            Nil
          }
          case ChoiceType(_) => {
            val decoupledName = parentName + "_" + name
            val decoupledMemberAssignments = decoupleMembers(decoupledName, _type)
            val redirectedType = redirectMembers(decoupledName, _type)
            val decoupledAssignment = TypeAssignment(TypeReference(decoupledName), redirectedType)
            decoupledAssignment::decoupledMemberAssignments
          }
          case SequenceType(spec) => {
            val decoupledName = parentName + "_" + name
            val decoupledMemberAssignments = decoupleMembers(decoupledName, _type)
            val redirectedType = redirectMembers(decoupledName, _type)
            val decoupledAssignment = TypeAssignment(TypeReference(decoupledName), redirectedType)
            decoupledAssignment::decoupledMemberAssignments
          }
          case INTEGER(None) => {
            Nil
          }
          case EnumeratedType(enumerations) => {
            val decoupledName = parentName + "_" + name
            val decoupledMemberAssignments = decoupleMembers(decoupledName, _type)
            val redirectedType = redirectMembers(decoupledName, _type)
            val decoupledAssignment = TypeAssignment(TypeReference(decoupledName), redirectedType)
            decoupledAssignment::decoupledMemberAssignments
          }
          case SetOfType(setElementType) => {
            val decoupledName = parentName + "_" + name
            val decoupledMemberAssignments = decoupleMembers(decoupledName, _type)
            val redirectedType = redirectMembers(decoupledName, _type)
            val decoupledAssignment = TypeAssignment(TypeReference(decoupledName), redirectedType)
            decoupledAssignment::decoupledMemberAssignments
          }
          case _: BuiltinType => Nil
          case _: UsefulType => Nil
          case _ =>
            println("Unprocessed in decoupleNamedType: " + typeKind)
            throw new Exception("Not implemented");
        }
      }
    }
  }
  
  def decouple(parentName: String, namedType: ast.NamedType): List[Assignment] = {
    namedType match { case ast.NamedType(Identifier(name), _type) =>
      decoupleNamedType(parentName, name, _type)
    }
  }
  
  def decoupleMembers(parentName: String, _type: Type): List[Assignment] = {
    val returnList = _type match {
      case Type(typeKind: TypeKind, constraints) => {
        typeKind match {
          case BitStringType(_) => Nil
          case EnumeratedType(e) => Nil
          case SetOfType(t) => {
            decoupleNamedType(parentName, "item", t)
          }
          case ChoiceType(
                AlternativeTypeLists(
                  RootAlternativeTypeList(
                    AlternativeTypeList(namedTypes)),
                  eae, eaa, oem)) => {
            var assignments = Nil: List[Assignment]
            namedTypes.flatMap { namedType => decouple(parentName, namedType) }
          }
          case INTEGER(maybeValues) if maybeValues != None => {
            println(_type)
            throw new Exception("Not implemented")
            Nil
          }
          case SequenceOfType(t) => {
            println(_type)
            throw new Exception("Not implemented")
            Nil // TODO
          }
          case SequenceType(ComponentTypeLists(maybeList1, extension, maybeList2)) => {
            var memberIndex = 0
            val newAssignments1 = maybeList1.toList.flatMap { list =>
              list.componentTypes.flatMap { componentType =>
                try {
                  componentType match {
                    case BasicComponentType(t) => {
                      decoupleNamedType(parentName, memberIndex.toString, t)
                    }
                    case NamedComponentType(ast.NamedType(Identifier(name), t), value) => {
                      decoupleNamedType(parentName, name, t)
                    }
                  }
                } finally {
                  memberIndex += 1
                }
              }
            }
            val newAssignments2 = maybeList2.toList.flatMap { list =>
              list.componentTypes.flatMap { componentType =>
                try {
                  componentType match {
                    case BasicComponentType(t) => {
                      decoupleNamedType(parentName, memberIndex.toString, t)
                    }
                    case NamedComponentType(ast.NamedType(Identifier(name), t), value) => {
                      decoupleNamedType(parentName, name, t)
                    }
                  }
                } finally {
                  memberIndex += 1
                }
              }
            }
            newAssignments1:::newAssignments2
          }
          case SequenceType(Empty) => Nil
          case SequenceType(spec) => {
            println("c ==> " + _type)
            throw new Exception("Not implemented")
            Nil // TODO
          }
          case SetType(spec) => {
            println("d ==> " + _type)
            throw new Exception("Not implemented")
            Nil // TODO
          }
          case TaggedType(tag, taggedKind, t) => {
            println("e ==> " + _type)
            throw new Exception("Not implemented")
            decoupleMembers(parentName, t)
          }
          case _: TypeReference => Nil
          case _: BuiltinType => Nil
          case _: UsefulType => Nil
          case _ => {
            println("f ==> " + _type)
            throw new Exception("Not implemented")
            Nil
          }
        }
      }
    }
    returnList
  }
  
  def redirect(parentName: String, _type: Type): Type = {
    _type match {
      case Type(typeKind, constraints) => {
        val decoupledTypeKind: TypeKind = typeKind match {
          case _: BitStringType => typeKind
          case _: EnumeratedType => TypeReference(parentName)
          case _: SetOfType => TypeReference(parentName)
          case ChoiceType(spec) => {
            TypeReference(parentName)
          }
          case INTEGER(Some(values)) => {
            throw new Exception("Not implemented")
            TypeReference(parentName)
          }
          case SequenceOfType(t) => {
            println(_type)
            throw new Exception("Not implemented")
            // TODO
          }
          case SequenceType(spec) => {
            TypeReference(parentName)
          }
          case _: SetType => TypeReference(parentName)
          case TaggedType(tag, taggedKind, t) => {
            val newType = redirectMembers(parentName, t)
            TaggedType(tag, taggedKind, newType)
          }
          case _: TypeReference => {
            typeKind
          }
          case _: BuiltinType => typeKind
          case _: UsefulType => typeKind
          case _ => {
            println(_type)
            throw new Exception("Not implemented")
            typeKind
          }
        }
        Type(decoupledTypeKind, constraints)
      }
    }
  }
  
  def redirectMembers(parentName: String, sequenceTypeSpec: SequenceTypeSpec): SequenceTypeSpec = {
    val newSequenceTypeSpec = sequenceTypeSpec match {
      case ComponentTypeLists(list1, extension, list2) => {
        var memberIndex = 0
        val newList1 = list1.map { list =>
          val newComponentTypes = list.componentTypes.map {
            case BasicComponentType(t) => {
              val newTypeName = parentName + "_" + memberIndex
              val redirectedType = redirect(newTypeName, t)
              BasicComponentType(redirectedType) // TODO
            }
            case NamedComponentType(ast.NamedType(Identifier(name), t@Type(tk, c)), value) => {
              val newTypeName = parentName + "_" + name
              val redirectedType = redirect(newTypeName, t)
              NamedComponentType(
                ast.NamedType(Identifier(name), redirectedType),
                value) // TODO
            }
          }
          memberIndex += 1
          list.copy(componentTypes = newComponentTypes)
        }
        val newList2 = list2.map { list =>
          println(sequenceTypeSpec)
          println(list2)
          throw new Exception("Not implemented")
          list
        }
        ComponentTypeLists(newList1, extension, newList2)
      }
      case Empty => Empty
    }
    newSequenceTypeSpec
  }
  
  def redirectMembers(parentName: String, alternativeTypeLists: AlternativeTypeLists): AlternativeTypeLists = {
    val newAlternativeTypeLists = alternativeTypeLists match {
      case AlternativeTypeLists(
          typeList,
          extensionAndException,
          extensionAdditionAlternatives,
          optionalExtensionMarker) => {
        var memberIndex = 0
        val newList1 = typeList.namedTypes.map {
          case ast.NamedType(Identifier(name), t@Type(tk, c)) => {
            val newTypeName = parentName + "_" + name
            val redirectedType = redirect(newTypeName, t)
            ast.NamedType(Identifier(name), redirectedType)
          }
        }
        AlternativeTypeLists(
            typeList,
            extensionAndException,
            extensionAdditionAlternatives,
            optionalExtensionMarker)
      }
    }
    newAlternativeTypeLists
  }
  
  def redirectMembers(parentName: String, _type: Type): Type = {
    _type match {
      case Type(typeKind, constraints) => {
        val decoupledTypeKind: TypeKind = typeKind match {
          case _: BitStringType => typeKind
          case _: EnumeratedType => typeKind
          case SetOfType(subType) => SetOfType(redirect(parentName + "_item", subType))
          case ChoiceType(spec) => ChoiceType(redirectMembers(parentName, spec))
          case INTEGER(Some(values)) => {
            throw new Exception("Not implemented")
            TypeReference(parentName)
          }
          case SequenceOfType(t) => {
            println(_type)
            throw new Exception("Not implemented")
            // TODO
          }
          case SequenceType(spec) => SequenceType(redirectMembers(parentName, spec))
          case _: SetType => TypeReference(parentName)
          case TaggedType(tag, taggedKind, t) => {
            val newType = redirectMembers(parentName, t)
            TaggedType(tag, taggedKind, newType)
          }
          case _: TypeReference => {
            typeKind
          }
          case OctetStringType => typeKind
          case _: BuiltinType => typeKind
          case _: UsefulType => typeKind
          case _ => {
            println(_type)
            throw new Exception("Not implemented")
            typeKind
          }
        }
        Type(decoupledTypeKind, constraints)
      }
    }
  }
}
