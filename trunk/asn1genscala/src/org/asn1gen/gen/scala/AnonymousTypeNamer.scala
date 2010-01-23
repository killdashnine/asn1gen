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
  
  def process(typeAssignment: TypeAssignment): List[Assignment] =
    process(typeAssignment.name, typeAssignment._type) match {
      case (newType, newAssignments) =>
        typeAssignment.copy(_type = newType)::newAssignments
    }
  
  def process(valueAssignment: ValueAssignment): List[Assignment] =
    process(valueAssignment.name, valueAssignment._type) match {
      case (newType, newAssignments) =>
        valueAssignment.copy(_type = newType)::newAssignments
    }
  
  def decouple(parentName: String, name: String, _type: Type): (Type, List[Assignment]) = {
    val newTypeName = parentName + "_" + name
    val replacementType = Type(TypeReference(newTypeName), Nil)
    process(newTypeName, _type) match { case (decoupledType, newAssignments) =>
      val newAssignment = TypeAssignment(TypeReference(newTypeName), decoupledType)
      (decoupledType, newAssignment::newAssignments)
    }
  }
  
  /**
   * @return (replacementType, newAssignments)
   */
  def processNamedType(parentName: String, name: String, _type: Type): (Type, List[Assignment]) = {
    _type match { case Type(typeKind, constraints) =>
      typeKind match {
        case TaggedType(tag: Tag, taggedKind, subType: Type) => {
          processNamedType(parentName, name, subType) match {
            case (newSubType, newAssignment) => {
              val replacementType = Type(TaggedType(tag, taggedKind, newSubType), constraints)
              (replacementType, newAssignment)
            }
          }
        }
        case typeReference: TypeReference => (_type, Nil)
        case SequenceType(spec) => decouple(parentName, name, _type)
        case _ =>
          println("Unprocessed in processNamedType: " + typeKind)
          throw new Exception("Not implemented");
      }
    }
  }
  
  def process(parentName: String, namedType: ast.NamedType): (ast.NamedType, List[Assignment]) = {
    namedType match { case ast.NamedType(Identifier(name), _type) =>
      processNamedType(parentName, name, _type) match { case (newType, newAssignments) =>
        (ast.NamedType(Identifier(name), newType), newAssignments)
      }
    }
  }
  
  def process(parentName: String, _type: Type): (Type, List[Assignment]) = {
    _type match {
      case Type(BitStringType(_), _) => (_type, Nil)
      case Type(EnumeratedType(e), _) => (_type, Nil)
      case Type(SetOfType(t), _) => (_type, Nil)
      case Type(
          ChoiceType(
            AlternativeTypeLists(
              RootAlternativeTypeList(
                AlternativeTypeList(namedTypes)),
              eae, eaa, oem)),
          constraint) => {
        var assignments = Nil: List[Assignment]
        val newList1 = namedTypes.map { namedType =>
          process(parentName, namedType) match { case (newNamedType, newAssignments) =>
            assignments = newAssignments:::assignments
            newNamedType
          }
        }
        val newType =
          Type(
            ChoiceType(
              AlternativeTypeLists(
                RootAlternativeTypeList(
                  AlternativeTypeList(namedTypes)),
                eae, eaa, oem)),
            constraint)
        (newType, assignments)
      }
      case Type(INTEGER(maybeValues), _) if maybeValues != None => {
        println(_type)
        throw new Exception("Not implemented")
        (_type, Nil) // TODO
      }
      case Type(SequenceOfType(t), _) => {
        println(_type)
        throw new Exception("Not implemented")
        (_type.copy(kind = SequenceOfType(t)), Nil) // TODO
      }
      case Type(SequenceType(ComponentTypeLists(list1, extension, list2)), _) => {
        var assignments = Nil: List[Assignment]
        var memberIndex = 0
        val newList1 = list1.map { list =>
          val newComponentTypes = list.componentTypes.map {
            case BasicComponentType(t) => {
              val newTypeName = parentName + "_" + memberIndex
              assignments = TypeAssignment(TypeReference(newTypeName), t)::assignments
              BasicComponentType(Type(TypeReference(newTypeName), Nil)) // TODO
            }
            case NamedComponentType(ast.NamedType(Identifier(name), t@Type(tk, c)), value) => {
              val newTypeName = parentName + "_" + name
              assignments = TypeAssignment(TypeReference(newTypeName), t)::assignments
              NamedComponentType(
                ast.NamedType(Identifier(name), Type(TypeReference(newTypeName), Nil)),
                value) // TODO
            }
          }
          memberIndex += 1
          list.copy(componentTypes = newComponentTypes)
        }
        val newList2 = list2.map { list =>
          println(_type)
          println(list2)
          throw new Exception("Not implemented")
          list
        }
        (_type.copy(kind = SequenceType(ComponentTypeLists(list1, extension, list2))), assignments) // TODO
      }
      case Type(SequenceType(Empty), _) => (_type, Nil)
      case Type(SequenceType(spec), _) => {
        println(_type)
        throw new Exception("Not implemented")
        (_type.copy(kind = SequenceType(spec)), Nil) // TODO
      }
      case Type(SetType(spec), _) => {
        println(_type)
        throw new Exception("Not implemented")
        (_type.copy(kind = SetType(spec)), Nil) // TODO
      }
      case Type(TaggedType(tag, taggedKind, t), _) => {
        println(_type)
        throw new Exception("Not implemented")
        process(parentName, t) match { case (newType, newAssignments) =>
          (_type.copy(kind = TaggedType(tag, taggedKind, newType)), newAssignments)
        }
      }
      case Type(_: TypeReference, _) => (_type, Nil)
      case Type(OctetStringType, _) => (_type, Nil)
      case Type(INTEGER(None), _) => (_type, Nil)
      case Type(REAL, _) => (_type, Nil)
      case Type(BOOLEAN, _) => (_type, Nil)
      case _ => {
        println(_type)
        throw new Exception("Not implemented")
        (_type, Nil)
      }
    }
  }
}
