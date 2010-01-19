package org.asn1gen.gen.scala

import org.asn1gen.parsing.asn1.ast._
import org.asn1gen.parsing.asn1.{ ast => ast }

trait AnonymousTypeNamer {
  def process(moduleDefinition: ModuleDefinition): ModuleDefinition =
    moduleDefinition.copy(moduleBody = moduleDefinition.moduleBody)
  
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
  
  def process(parentName: String, _type: Type): (Type, List[Assignment]) = {
    _type match {
      case Type(BitStringType(maybeFields), _) if (maybeFields != None) => {
        (_type.copy(kind = BitStringType(maybeFields)), Nil) // TODO
      }
      case Type(ChoiceType(typeLists), _) => {
        (_type.copy(kind = ChoiceType(typeLists)), Nil) // TODO
      }
      case Type(EnumeratedType(e), _) => {
        (_type.copy(kind = EnumeratedType(e)), Nil) // TODO
      }
      case Type(INTEGER(maybeValues), _) if maybeValues != None => (_type, Nil) // TODO
      case Type(SequenceOfType(t), _) => {
        (_type.copy(kind = SequenceOfType(t)), Nil) // TODO
      }
      case Type(SequenceType(ComponentTypeLists(list1, extension, list2)), _) => {
        var assignments = Nil: List[Assignment]
        val newList1 = list1.map { list =>
          val newComponentTypes = list.componentTypes.map {
            case BasicComponentType(t) => {
              BasicComponentType(t) // TODO
            }
            case NamedComponentType(ast.NamedType(Identifier(name), t@Type(tk, c)), value) => {
              val newTypeName = parentName + "_" + name
              assignments = TypeAssignment(TypeReference(newTypeName), t)::assignments
              NamedComponentType(ast.NamedType(Identifier(name), Type(TypeReference(newTypeName), Nil)), value) // TODO
            }
          }
          list.copy(componentTypes = newComponentTypes)
        }
        val newList2 = list2.map { list =>
          list
        }
        (_type.copy(kind = SequenceType(ComponentTypeLists(list1, extension, list2))), assignments) // TODO
      }
      case Type(SequenceType(spec), _) => {
        (_type.copy(kind = SequenceType(spec)), Nil) // TODO
      }
      case Type(SetOfType(t), _) => {
        (_type.copy(kind = SetOfType(t)), Nil) // TODO
      }
      case Type(SetType(spec), _) => {
        (_type.copy(kind = SetType(spec)), Nil) // TODO
      }
      case Type(TaggedType(tag, taggedKind, t), _) => {
        process(parentName, t) match { case (newType, newAssignments) =>
          (_type.copy(kind = TaggedType(tag, taggedKind, newType)), newAssignments)
        }
      }
      case _ => (_type, Nil)
    }
  }
}
