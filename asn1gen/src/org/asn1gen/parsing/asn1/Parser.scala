package org.asn1gen.parsing.asn1

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

import org.asn1gen.parsing.asn1.ast._
import org.asn1gen.parsing.syntax._

class Parser extends TokenParsers with ImplicitConversions {
  type Tokens = Lexer
  
  val lexical = new Tokens
  lexical.reserved +=
    ( "ABSENT", "ABSTRACT-SYNTAX", "ALL", "APPLICATION", "AUTOMATIC"
    , "BEGIN", "BIT", "BMPString", "BOOLEAN", "BY"
    , "CHARACTER", "CHOICE", "CLASS", "COMPONENT", "COMPONENTS", "CONSTRAINED", "CONTAINING"
    , "DEFAULT", "DEFINITIONS", "EMBEDDED", "ENCODED", "END", "ENUMERATED", "EXCEPT", "EXPLICIT", "EXPORTS", "EXTENSIBILITY", "EXTERNAL"
    , "FALSE", "FROM"
    , "GeneralizedTime", "GeneralString", "GraphicString"
    , "IA5String", "IDENTIFIER", "IMPLICIT", "IMPLIED", "IMPORTS", "INCLUDES", "INSTANCE", "INTEGER", "INTERSECTION", "ISO646String"
    , "MAX", "MIN", "MINUS-INFINITY"
    , "NULL", "NumericString"
    , "OBJECT", "ObjectDescriptor", "OCTET", "OF", "OPTIONAL"
    , "PATTERN", "PDV", "PLUS-INFINITY", "PRESENT", "PrintableString", "PRIVATE REAL"
    , "RELATIVE-OID"
    , "SEQUENCE", "SET SIZE", "STRING", "SYNTAX"
    , "T61String", "TAGS", "TeletexString", "TRUE", "TYPE-IDENTIFIER"
    , "UNION", "UNIQUE", "UNIVERSAL", "UniversalString", "UTCTime", "UTF8String"
    , "VideotexString", "VisibleString"
    , "WITH"
    )
  
  def elem[U](kind: String)(f: PartialFunction[Elem, U]) : Parser[U] = elem(kind, {_: Elem => true}) ^? f

  def op(chars: String) = elem("operator " + chars) {case lexical.Operator(`chars`) => Operator(chars)}
  def kw(chars: String) = elem("keyword " + chars) {case lexical.Keyword(`chars`) => Keyword(chars)}
  def empty = success("")

  // ASN1D 8.3.2<1-2>
  def bstring = elem("bstring") {
    case lexical.BString(s) => BString(s)
  }
  
  // ASN1D 8.3.2<3>
  // TODO: unused
  def comment = elem("comment") {
    case lexical.CommentLit(n) => n
  }
  
  // ASN1D: 8.2.3<4-5>
  // TODO: not implemented
  
  // ASN1D: 8.2.3<6-8>
  def cstring = elem("cstring") {
    case lexical.CString(s) => CString(s)
  }
  
  // ASN1D: 8.2.3<9>
  // TODO: not implemented

  // ASN1D: 8.2.3<10-11>
  def hstring = elem("hstring") {
    case lexical.HString(s) => HString(s)
  }
  
  // ASN1D: 8.2.3<12-14>
  def identifier = elem("identifier") {
    case lexical.Identifier(n) if n.first.isLowerCase => Identifier(n)
  }
  
  // ASN1D: 8.2.3<15-16>
  // TODO: not implemented
  
  // ASN1D: 8.2.3<17>
  def moduleReference =
    ( typeReference
    ) ^^ { case tr@TypeReference(_) => tr.asModuleReference }
  
  // ASN1D: 8.2.3<18>
  def number = elem("number") {
    case lexical.Number(s) => Number(s)
  }
  
  // ASN1D: 8.2.3<19>
  def objectClassReference =
    ( typeReference
    ) ^^ { case TypeReference(n) => ObjectClassReference(n) }
  
  // ASN1D: 8.2.3<20>
  // TODO: unsure if specification means there should be no space after '&'
  def objectFieldReference =
    ( valueFieldReference
    )
  
  // ASN1D: 8.2.3<21>
  def objectReference =
    ( lexical.Operator("&") ~> valueReference
    ) ^^ { case ValueReference(n) => ObjectFieldReference(n) }

  // ASN1D: 8.2.3<22>
  def objectSetFieldReference =
    ( lexical.Operator("&") ~> objectSetReference
    ) ^^ { case ObjectSetReference(n) => ObjectSetFieldReference(n) }

  // ASN1D: 8.2.3<23>
  def objectSetReference =
    ( typeReference
    ) ^^ { case TypeReference(n) => ObjectSetReference(n) }
  
  // ASN1D: 8.2.3<24>
  
  def signedNumber =
    ( ( lexical.Operator("-") ~ number
      ) ^? { case _ ~ number if number.chars != "0" =>
        SignedNumber(true, number)
      }
    | number ^^ { number =>
        SignedNumber(false, number)
      }
    )
  
  // ASN1D: 8.2.3<25>
  def typeFieldReference =
    ( lexical.Operator("&") ~> typeReference
    ) ^^ { case TypeReference(n) => TypeFieldReference(n) }
  
  // ASN1D: 8.2.3<26>
  /*def typeReference = elem(
    "type reference",
    { case lexical.Identifier(n) => n.first.isUpperCase}) ^^ {
      case lexical.Identifier(n) => TypeReference(n) 
    }*/
  def typeReference = elem("type reference") {
      case lexical.Identifier(n) if (n.first.isUpperCase) => TypeReference(n)
    } | failure ("incorrect type reference")

  // ASN1D: 8.2.3<27>
  // Not implemented

  // ASN1D: 8.2.3<28>
  // Implemented in lexer

  // ASN1D: 8.2.3<29>
  // Not applicable
  
  // ASN1D: 8.2.3<30>
  // Not implemented

  // ASN1D: 8.2.3<31>
  def valueFieldReference =
    ( lexical.Operator("&") ~> valueReference
    ) ^^ {
      case ValueReference(n) => ValueFieldReference(n)
    }

  // ASN1D: 8.2.3<31>
  def valueReference =
    ( identifier
    ) ^^ {
      case Identifier(n) => ValueReference(n)
    }

  // ASN1D: 8.2.3<33>
  def valueSetFieldReference =
    ( lexical.Operator("&") ~> typeReference
    ) ^^ {
      case TypeReference(n) => ValueSetFieldReference(n)
    }

  // ASN1D: 8.2.3<34-35>
  def word = elem(
    "type reference",
    { case lexical.Identifier(n) => !n.exists{_.isLowerCase}}) ^^ {
      case lexical.Identifier(n) => Word(n) 
    }

  // ASN1D: 8.2.3<36-37>
  // Not implemented

  // ASN1D: 9.1.2<1-2>
  def assignmentList =
    ( assignment.+
    ) ^^ { assignments => AssignmentList(assignments) }
  
  def assignment =
    ( typeAssignment
    | valueAssignment
    | valueSetTypeAssignment
    | objectClassAssignment
    | objectAssignment
    | objectSetAssignment
    | parameterizedAssignment
    ) ^^ { assignmentKind => Assignment(assignmentKind) }
  
  // ASN1D: 9.1.2<3>
  def opAssignment = lexical.Operator("::=")
  
  def typeAssignment =
    ( typeReference
    ~ opAssignment
    ~ `type`
    ) ^^ { case n ~ _ ~ t => TypeAssignment(n, t) }
  
  def `type` : Parser[Type] =
    ( builtinType
    | referencedType
    | constrainedType
    ) ^^ { kind => Type(kind) }
  
  def builtinType =
    ( bitStringType
    | booleanType
    | characterStringType
    | choiceType
    | embeddedPDVType
    | enumeratedType
    | externalType
    | instanceOfType
    | integerType
    | nullType
    | objectClassFieldType
    | objectIdentifierType
    | octetStringType
    | realType
    | relativeOidType
    | sequenceOfType
    | sequenceType
    | setOfType
    | setType
    | taggedType
    ) ^^ { kind => BuiltinType(kind) }
  
  def referencedType =
    ( definedType
    | usefulType
    | selectionType
    | typeFromObject
    | valueSetFromObjects
    ) ^^ { _ => ReferencedType() } // TODO
  
  // ASN1D 9.1.2<4>
  def valueAssignment =
    ( valueReference
    ~ `type`
    ~ opAssignment
    ~ value
    ) ^^ { _ => ValueAssignment() } // TODO
  
  // ASN1D 9.1.2<5>
  def value : Parser[Any] =
    ( builtinValue
    | referencedValue
    )
  
  // ASN1D 9.1.2<6>
  def builtinValue : Parser[Any] =
    ( bitStringValue
    | booleanValue
    | characterStringValue
    | choiceValue
    | embeddedPDVValue
    | enumeratedValue
    | externalValue
    | instanceOfValue
    | integerValue
    | nullValue
    | objectClassFieldValue
    | objectIdentifierValue
    | octetStringValue
    | realValue
    | relativeOidValue
    | sequenceValue
    | sequenceOfValue
    | setValue
    | setOfValue
    | taggedValue
    )
  
  def referencedValue =
    ( definedValue
    | valueFromObject
    )

  def taggedValue = value
  
  def valueSetTypeAssignment =
    ( typeReference
    ~ `type`
    ~ opAssignment
    ~ valueSet
    ) ^^ { _ => ValueSetTypeAssignment() } // TODO

  // ASN1D 9.1.2<7-9>
  // Not implemented
    
  // ASN1D 9.1.2<10>
  def objectClassAssignment =
    ( objectClassReference
    ~ opAssignment
    ~ objectClass
    ) ^^ { _ => ObjectClassAssignment() } // TODO
  
  def objectAssignment =
    ( objectReference
    ~ definedObjectClass
    ~ opAssignment
    ~ object_
    ) ^^ { _ => ObjectAssignment() } // TODO
  
  def objectSetAssignment =
    ( objectSetReference
    ~ definedObjectClass
    ~ opAssignment
    ~ objectSet
    ) ^^ { _ => ObjectSetAssignment() } // TODO
  
  def root = moduleDefinition
  
  // ASN1D 9.2.2<1>
  def moduleDefinition =
    ( moduleIdentifier
    ~ kw("DEFINITIONS")
    ~ tagDefault
    ~ extensionDefault
    ~ op("::=")
    ~ kw("BEGIN")
    ~ moduleBody
    ~ kw("END")
    ) ^^ { case mi ~ _ ~ td ~ ed ~ _ ~ _ ~ mb ~ _ =>
      ModuleDefinition(mi, td, ed, mb)
    }
  
  // ASN1D 9.2.2<2>
  def moduleIdentifier =
    ( moduleReference
    ~ definitiveIdentifier
    ) ^^ { case mr ~ di => ModuleIdentifier(mr, di) }
  
  // ASN1D 9.2.2<4>
  def definitiveIdentifier =
    ( op("{")
    ~ definitiveObjectIdComponent.+
    ~ op("}")
    ).? ^^ { _ => DefinitiveIdentifier() } /// TODO: populate
  
  // ASN1D 9.2.2<5>
  // Not implemented
    
  // ASN1D 9.2.2<6>
  def definitiveObjectIdComponent =
    ( failure("remove me")
    | nameForm
    | definitiveNumberForm
    | definitiveNameAndNumberForm
    )
  
  def definitiveNumberForm = number

  def nameForm = identifier
  
  def definitiveNameAndNumberForm =
    ( identifier
    ~ op("(")
    ~ definitiveNumberForm
    ~ op(")")
    )

  // ASN1D 9.2.2<7>
  // Not implemented
  
  // ASN1D 9.2.2<8>
  def extensionDefault =
    ( kw("EXTENSIBILITY") ~ kw("IMPLIED")
    | empty
    ) ^^ { _ => ExtensionDefault() }
  
  // ASN1D 9.2.2<8-11>
  // Not implemented
    
  // ASN1D 9.2.2<12>
  def moduleBody : Parser[ModuleBody] =
    ( exports
    ~ imports
    ~ assignmentList
    ).? ^^ {
      case Some(e ~ i ~ al) => ModuleBody(e, i, al)
      case None => new ModuleBody()
    }
  
  def exports =
    ( kw("EXPORTS")
    ~ symbolsExported
    ~ op(";")
    ).? ^^ { _ => Exports() }
  
  def symbolsExported = repsep(symbol, op(","))

  // ASN1D 9.2.2<13-15>
  // Not implemented
    
  // ASN1D 9.2.2<16>
  def imports =
    ( kw("IMPORTS")
    ~ symbolsImported
    ~ op(";")
    ).? ^^ { _ => Imports() }

  def symbolsImported =
    symbolsFromModule.*
  
  def symbolsFromModule =
    ( rep1sep(symbol, op(","))
    ~ kw("FROM")
    ~ globalModuleReference
    )
  
  // ASN1D 9.2.2<22>
  def globalModuleReference =
    ( moduleReference
    ~ assignedIdentifier
    )
  
  // ASN1D 9.2.2<27>
  def assignedIdentifier =
    ( objectIdentifierValue
    | definedValue
    | empty
    )

  // ASN1D 9.2.2<30>
  def symbol =
    ( reference
    | parameterizedReference
    )

  def reference =
    ( typeReference
    | valueReference
    | objectClassReference
    | objectReference
    | objectSetReference
    )
  
  def parameterizedReference =
    ( reference
    | reference ~ op("{") ~ op("}")
    )
  
  // ASN1D 9.3.2<1>
  def definedType =
    ( externalTypeReference
    | typeReference
    | parameterizedType
    | parameterizedValueSetType
    )

  // ASN1D 9.3.2<3>
  def externalTypeReference =
    ( moduleReference
    ~ op(".")
    ~ typeReference
    )
  
  // ASN1D 9.3.2<8>
  def definedValue =
    ( externalValueReference
    | valueReference
    | parameterizedValue
    ) ^^ { _ => DefinedValue() } // TODO
  
  // ASN1D 9.3.2<10>
  def externalValueReference =
    ( moduleReference
    ~ op(".")
    ~ valueReference
    )
  
  // ASN1D 9.3.2<14>
  def definedObjectClass =
    ( externalObjectClassReference
    | objectClassReference
    | usefulObjectClassReference
    )
  
  // ASN1D 9.3.2<15>
  def externalObjectClassReference =
    ( moduleReference
    ~ op(".")
    ~ objectClassReference
    )
  
  // ASN1D 9.3.2<19>
  def definedObject =
    ( externalObjectReference
    | objectReference
    )
  
  // ASN1D 9.3.2<20>
  def externalObjectReference =
    ( moduleReference
    ~ op(".")
    ~ objectReference
    )
  
  // ASN1D 9.3.2<24>
  def definedObjectSet =
    ( externalObjectSetReference
    | objectSetReference
    )
  
  def externalObjectSetReference =
    ( moduleReference
    ~ op(".")
    ~ objectSetReference
    )
  
  // ASN1D 10.1.2
  def booleanType = kw("BOOLEAN") ^^ { _ => BooleanType() } // TODO
  
  def booleanValue =
    ( kw("TRUE") ^^ { _ => BooleanValue(true) }
    | kw("FALSE") ^^ { _ => BooleanValue(false) }
    )
  
  // ASN1D 10.2.2
  def nullType = kw("NULL") ^^ { _ => NullType() } // TODO
  
  def nullValue = kw("NULL")
  
  // ASN1D 10.3.2<1>
  def integerType =
    ( kw("INTEGER")
    ~ ( op("{")
      ~ rep1sep(namedNumber, op(","))
      ~ op("}")
      ).?
    ) ^^ {
      case _ ~ None => IntegerType(Nil)
      case _ ~ Some(_ ~ namedNumbers ~ _) => IntegerType(namedNumbers)
    }
  
  // ASN1D 10.3.2<6>
  def namedNumber =
    ( ( identifier
      ~ op("(")
      ~ signedNumber
      ~ op(")")
      ) ^^ { case id ~ _ ~ sn ~ _ =>
        NamedNumber(id, sn)
      }
    | ( identifier
      ~ op("(")
      ~ definedValue
      ~ op(")")
      ) ^^ { case id ~ _ ~ dv ~ _ =>
        NamedNumber(id, dv)
      }
    )
  
  // ASN1D 10.3.2<11>
  def integerValue =
    ( signedNumber
    | identifier
    )
  
  // ASN1D 10.4.2<1>
  def enumeratedType =
    ( kw("ENUMERATED")
    ~ op("{")
    ~ enumerations
    ~ op("}")
    ) ^^ { _ => EnumeratedType() } // TODO
  
  // ASN1D 10.4.2<5>
  def enumerations =
    ( rootEnumeration
    | ( rootEnumeration
      ~ op(",")
      ~ op("...")
      ~ exceptionSpec
      )
    | ( rootEnumeration
      ~ op(",")
      ~ op("...")
      ~ exceptionSpec
      ~ op(",")
      ~ additionalEnumeration
      )
    )
  
  // ASN1D 10.4.2<7>
  def rootEnumeration = enumeration

  // ASN1D 10.4.2<8>
  def additionalEnumeration = enumeration

  // ASN1D 10.4.2<11>
  def enumeration =
    ( rep1sep(enumerationItem, op(","))
    )
  
  def enumerationItem = identifier | namedNumber
  
  // ASN1D 10.4.2<13>
  // See 10.3.2<6>
  // Not implemented
  
  // ASN1D 10.4.2<16>
  def enumeratedValue = identifier
  
  // ASN1D 10.5.2<1>
  def realType = kw("REAL") ^^ { _ => RealType() }
  
  // ASN1D 10.5.2<5>
  def realValue =
    ( numericRealValue
    | specialRealValue
    )
  
  def numericRealValue =
    ( op("0") // TODO: Not going to be picked up by lexer
    | sequenceValue
    )
  
  // ASN1D 10.5.2<8>
  def specialRealValue =
    ( kw("PLUS-INFINITY")
    | kw("MINUS-INFINITY")
    )
  
  // ASN1D 10.6.2<1>
  def bitStringType =
    ( ( kw("BIT")
      ~ kw("STRING")
      )
    | ( kw("BIT")
      ~ kw("STRING")
      ~ op("{")
      ~ rep1sep(namedBit, op(","))
      ~ op("}")
      )
    ) ^^ { _ => BitStringType() } // TODO
  
  // ASN1D 10.6.2<6>
  def namedBit =
    ( ( identifier
      ~ op("(")
      ~ number
      ~ op(")")
      )
    | ( identifier
      ~ op("(")
      ~ definedValue
      ~ op(")")
      )
    )
  
  // ASN1D 10.6.2<13>
  def bitStringValue =
    ( bstring
    | hstring
    | identifierList
    )
  
  // ASN1D 10.6.2<16>
  def identifierList =
    ( op("{")
    ~ repsep(identifier, op(","))
    ~ op("}")
    )
  
  // ASN1D 10.7.2<1>
  def octetStringType =
    ( kw("OCTET")
    ~ kw("STRING")
    ) ^^ { _ => OctetStringType() } // TODO
  
  // ASN1D 10.7.2<4>
  def octetStringValue =
    ( bstring
    | hstring
    )
  
  // ASN1D 10.8.2<1>
  def objectIdentifierType =
    ( kw("OBJECT")
    ~ kw("IDENTIFIER")
    ) ^^ { _ => ObjectIdentifierType() } // TODO
  
  // ASN1D 10.8.2<3>
  def objectIdentifierValue =
    ( ( op("{")
      ~ objIdComponents.+
      ~ op("}")
      )
    | ( op("{")
      ~ definedValue
      ~ objIdComponents.+
      ~ op("}")
      )
    )
  
  // ASN1D 10.8.2<5>
  def objIdComponents =
    ( nameForm
    | numberForm
    | nameAndNumberForm
    | definedValue
    )
  
  // ASN1D 10.8.2<6>
  // See ASN1D 9.2.2<6>
  
  // ASN1D 10.8.6<8>
  def numberForm =
    ( number
    | definedValue
    )
  
  // ASN1D 10.8.6<10>
  def nameAndNumberForm =
    ( identifier
    ~ op("(")
    ~ numberForm
    ~ op(")")
    )
  
  // ASN1D 10.9.2<2>
  def relativeOidType =
    kw("RELATIVE-OID") ^^ { _ => RelativeOidType() } // TODO
  
  // ASN1D 10.9.2<4>
  def relativeOidValue =
    ( op("{")
    ~ relativeOidComponents.+
    ~ op("}")
    )
  
  def relativeOidComponents =
    ( numberForm
    | nameAndNumberForm
    | definedValue
    )
  
  // ASN1D 10.9.2<5>
  // See ASN1D 10.8.6<8>
  /*def numberForm =
    ( number
    | definedValue
    )*/
  
  // ASN1D 10.9.2<7>
  // See ASN1D 10.8.6<10>
  
  // ASN1D 11.10.2<9>
  def restrictedCharacterStringValue =
    ( cstring
    | characterStringList
    | quadruple
    | tuple
    )
  
  def characterStringList =
    ( op("{")
    ~ rep1sep(charsDefn, op(","))
    ~ op("}")
    )
  
  def charsDefn =
    ( cstring
    | quadruple
    | tuple
    | definedValue
    )
  
  // ASN1D 11.10.2<9>
  def quadruple =
    ( op("{") ~ group
    ~ op(",") ~ plane
    ~ op(",") ~ row
    ~ op(",") ~ cell
    ~ op("}")
    )

  def group = number
  def plane = number
  def row = number
  def cell = number
  
  // ASN1D 11.13<1>
  def characterStringType =
    ( restrictedCharacterStringType
    | unrestrictedCharacterStringType
    ) ^^ { _ => CharacterStringType() } // TODO

  def restrictedCharacterStringType =
    ( kw("BMPString")
    | kw("GeneralString")
    | kw("IA5String")
    | kw("NumericString")
    | kw("TeletexString")
    | kw("UniversalString")
    | kw("VideotexString")
    | kw("GraphicString")
    | kw("ISO646String")
    | kw("PrintableString")
    | kw("T61String")
    | kw("UTF8String")
    | kw("VisibleString")
    )

  // ASN1D 11.13<36>
  def characterStringValue =
    ( restrictedCharacterStringValue
    | unrestrictedCharacterStringValue
    )
  
  // See ASN1D 11.10.2<9>
  
  // ASN1D 11.13<38>
  // See ASN1D 11.10.2<9>
  
  // ASN1D 11.13<39>
  // See ASN1D 11.10.2<9>
  
  // ASN1D 11.13<40>
  // See ASN1D 11.10.2<9>
  def tuple =
    ( op("{")
    ~ tableColumn
    ~ op(",")
    ~ tableRow
    ~ op("}")
    )
  
  def tableColumn = number
  
  def tableRow = number
  
  // ASN1D 11.15.2
  def usefulType =
    ( kw("GeneralizedTime")
    | kw("UTCTime")
    | kw("ObjectDescriptor")
    )
  
  // ASN1D 11.16.2<1>
  // See ASN1D 11.15.2<1>
  
  // ASN1D 11.17.2<1>
  // See ASN1D 11.15.2<1>

  // ASN1D 12.1.4<1>
  def taggedType =
    ( ( tag
      ~ `type`
      ) ^^ { case tag ~ t => DefaultTaggedType(tag, t) }
    | ( tag
      ~ kw("IMPLICIT")
      ~ `type`
      ) ^^ { case tag ~ _ ~ t => ImplicitTaggedType(tag, t) }
    | ( tag
      ~ kw("EXPLICIT")
      ~ `type`
      ) ^^ { case tag ~ _ ~ t => ExplicitTaggedType(tag, t) }
    )
  
  def tag =
    ( op("[")
    ~ class_
    ~ classNumber
    ~ op("]")
    ) ^^ { case _ ~ c ~ cn ~ _ => Tag(c, cn) }
  
  def classNumber =
    ( number ^^ { n => LiteralClassNumber(n) }
    | definedValue ^^ { dv => DefinedClassNumber(dv) }
    )
  
  // ASN1D 12.1.4<7>
  def class_ =
    ( kw("UNIVERSAL") ^^ { _ => UniversalClass() }
    | kw("APPLICATION") ^^ { _ => ApplicationClass() }
    | kw("PRIVATE") ^^ { _ => PrivateClass() }
    | empty ^^ { _ => DefaultClass() }
    )
  
  // ASN1D 12.1.4<15>
  // See ASN1D 9.2.2<1>
  def tagDefault =
    ( kw("EXPLICIT") ~ kw("TAGS")
    | kw("IMPLICIT") ~ kw("TAGS")
    | kw("AUTOMATIC") ~ kw("TAGS")
    | empty
    ) ^^ { _ => TagDefault() }

  // ASN1D 12.2.2<1>
  def sequenceType =
    ( ( kw("SEQUENCE")
      ~ op("{")
      ~ op("}")
      )
    | ( kw("SEQUENCE")
      ~ op("{")
      ~ extensionAndException
      ~ optionalExtensionMarker
      ~ op("}")
      )
    | ( kw("SEQUENCE")
      ~ op("{")
      ~ componentTypeLists
      ~ op("}")
      )
    ) ^^ { _ => SequenceType() } // TODO
  
  // ASN1D 12.2.2<4>
  def componentTypeLists =
    ( rootComponentTypeList
    | ( rootComponentTypeList
      ~ op(",")
      ~ extensionAndException
      ~ extensionsAdditions
      ~ optionalExtensionMarker
      )
    | ( rootComponentTypeList
      ~ op(",")
      ~ extensionAndException
      ~ extensionsAdditions
      ~ optionalExtensionMarker
      ~ op(",")
      ~ rootComponentTypeList
      )
    | ( extensionAndException
      ~ extensionsAdditions
      ~ optionalExtensionMarker
      ~ op(",")
      ~ rootComponentTypeList
      )
    )
  
  // ASN1D 12.2.2<5>
  def rootComponentTypeList = componentTypeList
  def extensionsAdditions =
    ( ( op(",")
      ~ extensionAdditionList
      )
    | empty
    )

  def extensionAdditionList = rep1sep(extensionAddition, op(","))
  def extensionAddition = componentType ~ extensionAdditionGroup
  def extensionAdditionGroup = op("[[") ~ componentTypeList ~ op("]]")
  def componentTypeList = rep1sep(componentType, op(","))
  
  // ASN1D 12.2.2<13>
  def componentType =
    ( namedType
    | namedType ~ kw("OPTIONAL")
    | namedType ~ kw("DEFAULT") ~ value
    | kw("COMPONENTS") ~ kw("OF") ~ `type`
    )
  
  // ASN1D 12.2.2<24>
  def namedType =
    ( identifier ~ `type`
    ) ^^ { case id ~ t => NamedType(id, t) }

  // ASN1D 12.2.2<25>
  def sequenceValue =
    ( op("{")
    ~ repsep(namedValue, op(","))
    ~ op("}")
    )

  // ASN1D 12.2.2<31>
  def namedValue = identifier ~ value

  // ASN1D 12.3.2<1>
  def setType =
    ( kw("SET") ~ op("{") ~ op("}")
    | kw("SET") ~ op("{") ~ extensionAndException ~ optionalExtensionMarker ~ op("}")
    | kw("SET") ~ op("{") ~ componentTypeLists ~ op("}")
    ) ^^ { _ => SetType() } // TODO

  // ASN1D 12.3.2<4>
  // See ASN1D 12.2.2<4>
  
  // ASN1D 12.3.2<5>
  // See ASN1D 12.2.2<5>
  
  // ASN1D 12.3.2<13>
  // See ASN1D 12.2.2<13>
  
  // ASN1D 12.3.2<25>
  def setValue =
    ( op("{")
    ~ repsep(namedValue, op(","))
    ~ op("}")
    )

  // ASN1D 12.3.2<31>
  // See ASN1D 12.2.2<31>

  // ASN1D 12.4.2<1>
  def sequenceOfType =
    ( kw("SEQUENCE") ~ kw("OF") ~ `type`
    ) ^^ { _ => SequenceOfType() } // TODO

  // ASN1D 12.4.2<3>
  def typeWithConstraint =
    ( kw("SEQUENCE") ~ constraint ~ kw("OF") ~ `type`
    | kw("SEQUENCE") ~ sizeConstraint ~ kw("OF") ~ `type`
    | kw("SET") ~ constraint ~ kw("OF") ~ `type`
    | kw("SET") ~ sizeConstraint ~ kw("OF") ~ `type`
    )
  
  // ASN1D 12.4.2<5>
  def sizeConstraint = kw("SIZE") ~ constraint

  // ASN1D 12.4.2<8>
  def sequenceOfValue = op("{") ~ repsep(value, op(",")) ~ op("}")

  // ASN1D 12.5.2<1>
  def setOfType =
    ( kw("SET") ~ kw("OF") ~ `type`
    ) ^^ { _ => SetOfType() } // TODO

  // ASN1D 12.5.2<3>
  // See ASN1D 12.4.2<3>
  
  // ASN1D 12.5.2<5>
  // See ASN1D 12.4.2<5>

  // ASN1D 12.5.2<8>
  def setOfValue = op("{") ~ repsep(value, op(",")) ~ op("}")

  // ASN1D 12.6.2<1>
  def choiceType =
    ( kw("CHOICE") ~ op("{") ~ alternativeTypeLists ~ op("}")
    ) ^^ { case _ ~ _ ~ typeLists ~ _ => ChoiceType(typeLists) }

  // ASN1D 12.6.2<3>
  def alternativeTypeLists =
    ( ( rootAlternativeTypeList
      ) ^^ { ratl => AlternativeTypeLists(ratl, None, None, None) }
    | ( rootAlternativeTypeList
      ~ op(",")
      ~ extensionAndException
      ~ extensionAdditionAlternatives
      ~ optionalExtensionMarker
      ) ^^ { case ratl ~ _ ~ eae ~ eaa ~ oem => AlternativeTypeLists(ratl, Some(eae), Some(eaa), Some(oem)) }
    )
  
  // ASN1D 12.6.2<4>
  def rootAlternativeTypeList =
    alternativeTypeList ^^ { atl => RootAlternativeTypeList(atl) }
  def extensionAdditionAlternatives =
    ( op(",") ~ extensionAdditionAlternativesList
    | empty
    ) ^^ { _ => ExtensionAdditionAlternatives() } // TODO

  def extensionAdditionAlternativesList =
    rep1sep(extensionAdditionAlternative, op(","))

  def extensionAdditionAlternative =
    ( extensionAdditionGroupAlternatives
    | namedType
    )

  def extensionAdditionGroupAlternatives =
    ( op("[[")
    ~ alternativeTypeList
    ~ op("]]")
    )

  def alternativeTypeList = rep1sep(namedType, op(","))
  
  // ASN1D 12.6.1<10>
  // See ASN1D 12.2.2<24>

  // ASN1D 12.6.1<14>
  def choiceValue = identifier ~ op(":") ~ value

  // ASN1D 12.7.2<1>
  def selectionType = identifier ~ op("<") ~ `type`

  // ASN1D 12.9.2<1>
  def extensionAndException =
    ( op("...")
    | op("...") ~ exceptionSpec
    ) ^^ { _ => ExtensionAndException() } // TODO

  def optionalExtensionMarker =
    ( op(",") ~ op("...")
    | empty
    ) ^^ { _ => OptionalExtensionMarker() } // TODO

  def extensionEndMarker = op(",") ~ op("...")
  
  // ASN1D 12.9.2<18>
  def exceptionSpec = op("!") ~ exceptionIdentification | empty

  // ASN1D 12.9.2<20>
  def exceptionIdentification = signedNumber | definedValue | `type` ~ op(":") ~ value
  
  // ASN1D 13.1.2<1>
  def constrainedType =
    ( `type` ~ constraint
    | typeWithConstraint
    ) ^^ { _ => ConstrainedType() } // TODO

  // ASN1D 13.2.2<1>
  def singleValue = value

  // ASN1D 13.3.2<1>
  def containedSubtype = includes ~ `type`

  // ASN1D 13.3.7<7>
  def includes = kw("INCLUDES") | empty

  // ASN1D 13.4.2<1>
  def valueRange = lowerEndPoint ~ op("..") ~ upperEndPoint

  // ASN1D 13.4.2<3>
  def lowerEndPoint = lowerEndValue | lowerEndValue ~ op("<")
  def upperEndPoint = upperEndValue | op("<") ~ upperEndValue
  
  // ASN1D 13.4.2<4>
  def lowerEndValue = value | kw("MIN")
  def upperEndValue = value | kw("MAX")
  
  // ASN1D 13.5.2<1>
  // See ASN1D 12.4.2<5>

  // ASN1D 13.5.2<5>
  // See ASN1D 12.4.2<3>
  
  // ASN1D 13.6.2<1>
  def permittedAlphabet = kw("FROM") ~ constraint

  // ASN1D 13.6.2<5>
  // See ASN1D 13.4.2<1>

  // ASN1D 13.6.2<9>
  // See ASN1D 13.4.2<3>
  
  // ASN1D 13.6.2<10>
  // See ASN1D 13.4.2<4>
  
  // ASN1D 13.7.2<1>
  def patternConstraint = kw("PATTERN") ~ value

  // ASN1D 13.8.2<1>
  def innerTypeConstraints =
    ( kw("WITH") ~ kw("COMPONENT") ~ singleTypeConstraint
    | kw("WITH") ~ kw("COMPONENTS") ~ multipleTypeConstraints
    )
  
  // ASN1D 13.8.2<3>
  def singleTypeConstraint = constraint

  // ASN1D 13.9.2<1>
  // See ASN1D 13.8.2<1>
  
  // ASN1D 13.9.2<4>
  def multipleTypeConstraints = fullSpecification | partialSpecification

  // ASN1D 13.9.2<5>
  def fullSpecification = op("{") ~ typeConstraints ~ op("}")

  // ASN1D 13.9.2<7>
  def partialSpecification = op("{") ~ op("...") ~ op(",") ~ typeConstraints ~ op("}")

  // ASN1D 13.9.2<9>
  def typeConstraints = rep1sep(namedConstraint, op(","))

  // ASN1D 13.9.2<10>
  def namedConstraint = identifier ~ componentConstraint

  // ASN1D 13.9.2<12>
  def componentConstraint = valueConstraint ~ presenceConstraint
  def valueConstraint = constraint | empty
  
  // ASN1D 13.9.2<14>
  def presenceConstraint = kw("PRESENT") | kw("ABSENT") | kw("OPTIONAL") | empty
  
  // ASN1D 13.10.2<1>
  def contentsConstraint =
    ( kw("CONTAINING") ~ `type`
    | kw("ENCODED") ~ kw("BY") ~ value
    | kw("CONTAINING") ~ `type` ~ kw("ENCODED") ~ kw("BY") ~ value
    )
  
  // ASN1D 13.11.2<1>
  def elementSetSpecs : Parser[Any] =
    ( rootElementSetSpec
    | rootElementSetSpec ~ op(",") ~ op("...")
    | rootElementSetSpec ~ op(",") ~ op("...") ~ op(",") ~ additionalElementSetSpec
    )
  
  // ASN1D 13.11.2<2>
  def rootElementSetSpec : Parser[Any] = elementSetSpec
  def additionalElementSetSpec : Parser[Any] = elementSetSpec
  
  // ASN1D 13.11.2<9>
  def elementSetSpec : Parser[Any] = unions | kw("ALL") ~ exclusions

  // ASN1D 13.11.2<10>
  def unions : Parser[Any] = intersections | uElems ~ unionMark ~ intersections

  // ASN1D 13.11.2<11>
  def uElems = unions
  def unionMark = op("|") | kw("UNION")
 
  // ASN1D 13.11.2<12>
  def intersections : Parser[Any] = intersectionElements | iElems ~ intersectionMark ~ intersectionElements
  
  // ASN1D 13.11.2<13>
  def iElems = intersections
  def intersectionMark = op("^") | kw("INTERSECTION")
  def intersectionElements = elements | elems ~ exclusions
  
  // ASN1D 13.11.2<14>
  def elems = elements
  def exclusions = kw("EXCEPT") ~ elements

  // ASN1D 13.11.2<16>
  def elements = subtypeElements | objectSetElements | op("(") ~ elementSetSpec ~ op(")")
  
  // ASN1D 13.11.2<17>
  def subtypeElements =
    ( singleValue
    | containedSubtype
    | valueRange
    | permittedAlphabet
    | sizeConstraint
    | typeConstraint
    | innerTypeConstraints
    | patternConstraint
    )
  
  // ASN1D 13.12.2<1>
  def constraint : Parser[Any] = op("(") ~ constraintSpec ~ exceptionSpec ~ op(")")

  // ASN1D 13.12.2<5>
  def constraintSpec = elementSetSpecs | generalConstraint

  // ASN1D 13.12.2<6>
  // See ASN1D 13.11.2<1>

  // ASN1D 13.13.2<1>
  def userDefinedConstraint =
    ( kw("CONSTRAINED")
    ~ kw("BY")
    ~ op("{")
    ~ repsep(userDefinedConstraintParameter, op(","))
    ~ op("}")
    )
  
  // ASN1D 13.13.2<3>
  def userDefinedConstraintParameter	=
    ( governor ~ op(":") ~ value
    | governor ~ op(":") ~ valueSet
    | governor ~ op(":") ~ object_
    | governor ~ op(":") ~ objectSet
    | `type`
    | definedObjectClass
    )
  
  // ASN1D 13.13.2<7>
  def governor =
    ( `type`
    | definedObjectClass
    )

  // ASN1D 14.1.2<1>
  def externalType = kw("EXTERNAL") ^^ { _ => ExternalType() } // TODO

  // ASN1D 14.1.2<7>
  def externalValue = sequenceValue

  // ASN1D 14.2.2<1>
  def embeddedPDVType =
    ( kw("EMBEDDED") ~ kw("PDV")
    ) ^^ { _ => EmbeddedPDVType() } // TODO

  // ASN1D 14.2.2<5>
  def embeddedPDVValue = sequenceValue

  // ASN1D 14.3.2<1>
  def unrestrictedCharacterStringType = kw("CHARACTER") ~ kw("STRING")

  // ASN1D 14.3.2<6>
  def unrestrictedCharacterStringValue = sequenceValue

  // ASN1D 15.2.2<1>
  def objectClass = definedObjectClass | objectClassDefn | parameterizedObjectClass
  def objectClassDefn =
    ( kw("CLASS")
    ~ op("{")
    ~ rep1sep(fieldSpec, op(","))
    ~ withSyntaxSpec
    )

  def fieldSpec =
    ( typeFieldSpec
    | fixedTypeValueFieldSpec
    | variableTypeValueFieldSpec
    | fixedTypeValueSetFieldSpec
    | variableTypeValueSetFieldSpec
    | objectFieldSpec
    | objectSetFieldSpec
    )
  
  // ASN1D 15.2.2<3>
  def typeFieldSpec = typeFieldReference ~ typeOptionalitySpec
  def typeOptionalitySpec = kw("OPTIONAL") | kw("DEFAULT") ~ `type` | empty
  
  // ASN1D 15.2.2<6>
  def fixedTypeValueFieldSpec = valueFieldReference ~ `type` ~ unique ~ valueOptionalitySpec

  // ASN1D 15.2.2<7>
  def unique = kw("UNIQUE") | empty

  // ASN1D 15.2.2<10>
  def valueOptionalitySpec = kw("OPTIONAL") | kw("DEFAULT") ~ value | empty
  
  // ASN1D 15.2.2<13>
  def variableTypeValueFieldSpec = valueFieldReference ~ fieldName ~ valueOptionalitySpec

  // ASN1D 15.2.2<17>
  def fixedTypeValueSetFieldSpec = valueSetFieldReference ~ `type` ~ valueSetOptionalitySpec

  // ASN1D 15.2.2<18>
  def valueSetOptionalitySpec = kw("OPTIONAL") | kw("DEFAULT") ~ valueSet | empty
  
  // ASN1D 15.2.2<21>
  def variableTypeValueSetFieldSpec = valueSetFieldReference ~ fieldName ~ valueSetOptionalitySpec

  // ASN1D 15.2.2<25>
  def objectFieldSpec = objectFieldReference ~ definedObjectClass ~ objectOptionalitySpec

  // ASN1D 15.2.2<26>
  def objectOptionalitySpec = kw("OPTIONAL") | kw("DEFAULT") ~ object_ | empty
  
  // ASN1D 15.2.2<28>
  def objectSetFieldSpec = objectSetFieldReference ~ definedObjectClass ~ objectSetOptionalitySpec
  
  // ASN1D 15.2.2<29>
  def objectSetOptionalitySpec = kw("OPTIONAL") | kw("DEFAULT") ~ objectSet | empty
  
  // ASN1D 15.2.2<33>
  def fieldName = rep1sep(primitiveFieldName, op("."))
  def primitiveFieldName =
    ( typeFieldReference
    | valueFieldReference
    | valueSetFieldReference
    | objectFieldReference
    | objectSetFieldReference
    )

  // ASN1D 15.2.2<34>
  def object_ : Parser[Any] = objectDefn	| definedObject | objectFromObject | parameterizedObject

  // ASN1D 15.2.2<35>
  def objectDefn = defaultSyntax | definedSyntax

  // ASN1D 15.2.2<36>
  def defaultSyntax = op("{") ~ repsep(fieldSetting,op(",")) ~ op("}")
  def fieldSetting = primitiveFieldName ~ setting

  // ASN1D 15.2.2<38>
  def setting = `type` | value | valueSet | object_ | objectSet
  
  // ASN1D 15.3.2<1>
  def withSyntaxSpec = kw("WITH") ~ kw("SYNTAX") ~ syntaxList | empty

  // ASN1D 15.3.2<2>
  def syntaxList = op("{") ~ tokenOrGroupSpec.+ ~ op("}")
  def tokenOrGroupSpec : Parser[Any] = requiredToken | optionalGroup
  def requiredToken = literal | primitiveFieldName
  
  // ASN1D 15.3.2<3>
  def optionalGroup = op("[") ~ tokenOrGroupSpec.+ ~ op("]")

  // ASN1D 15.3.2<8>
  def literal = word | op(",")

  // ASN1D 15.3.2<11>
  def definedSyntax = op("{") ~ definedSyntaxToken.* ~ op("}")
  def definedSyntaxToken = literal
  // See ASN1D 15.3.2<8>
  // See ASN1D 15.2.2<38>
  
  // ASN1D 15.5.2<1>
  def objectSet = op("{") ~ objectSetSpec ~ op("}")

  // ASN1D 15.5.2<2>
  def objectSetSpec =
    ( rootElementSetSpec
    | rootElementSetSpec ~ op(",") ~ op("...")
    | rootElementSetSpec ~ op(",") ~ op("...") ~ op(",") ~ additionalElementSetSpec
    | op("...")
    | op("...") ~ op(",") ~ additionalElementSetSpec
    )
  
  // ASN1D 15.5.2<11>
  def valueSet = op("{") ~ elementSetSpecs ~ op("}")
  // See ASN1D 13.12.2<6>
  
  // ASN1D 15.5.2<22>
  // See ASN1D 13.11.2<2>

  // ASN1D 15.5.2<25>
  // See ASN1D 13.11.2<9>
  
  // ASN1D 15.5.2<26>
  // See ASN1D 13.11.2<10>

  // ASN1D 15.5.2<27>
  // See ASN1D 13.11.2<11>
  
  // ASN1D 15.5.2<28>
  // See ASN1D 13.11.2<12>
  
  // ASN1D 15.5.2<29>
  // See ASN1D 13.11.2<13>
  
  // ASN1D 15.5.2<30>
  // See ASN1D 13.11.2<14>

  // ASN1D 15.5.2<31>
  // See ASN1D 13.11.2<16>
  
  // ASN1D 15.5.2<34>
  def objectSetElements = object_ | definedObjectSet | objectSetFromObjects | parameterizedObjectSet
  
  // ASN1D 15.6.2<1>
  def valueFromObject = referencedObjects ~ op(".") ~ fieldName

  // ASN1D 15.6.2<5>
  def valueSetFromObjects = referencedObjects ~ op(".") ~ fieldName

  // ASN1D 15.6.2<10>
  def typeFromObject = referencedObjects ~ op(".") ~ fieldName

  // ASN1D 15.6.2<13>
  def objectFromObject = referencedObjects ~ op(".") ~ fieldName

  // ASN1D 15.6.2<15>
  def objectSetFromObjects = referencedObjects ~ op(".") ~ fieldName

  // ASN1D 15.6.2<19>
  def referencedObjects = definedObject	| parameterizedObject | definedObjectSet | parameterizedObjectSet
  // See ASN1D 15.2.2<33>
  
  // ASN1D 15.6.2<20>
  // See ASN1D 15.2.2<33>

  // ASN1D 15.7.2<1>
  def objectClassFieldType =
    ( definedObjectClass ~ op(".") ~ fieldName
    ) ^^ { _ => ObjectClassFieldType() } // TODO
  // See ASN1D 15.2.2<33>
  
  // ASN1D 15.7.2<9>
  def objectClassFieldValue = openTypeFieldVal | fixedTypeFieldVal

  // ASN1D 15.7.2<11>
  def openTypeFieldVal = `type` ~ op(":") ~ value

  // ASN1D 15.7.2<13>
  def fixedTypeFieldVal = builtinValue | referencedValue

  // ASN1D 15.7.2<15>
  // See ASN1D 13.12.2<1>

  // ASN1D 15.7.2<16>
  // See ASN1D 13.12.2<5>
  def generalConstraint = userDefinedConstraint | tableConstraint | contentsConstraint
  def tableConstraint = simpleTableConstraint | componentRelationConstraint
  
  // ASN1D 15.7.2<17>
  def simpleTableConstraint = objectSet

  // ASN1D 15.7.2<22>
  def componentRelationConstraint =	
    ( op("{")
    ~ definedObjectSet
    ~ op("}")
    ~ op("{")
    ~ rep1sep(atNotation, op(","))
    ~ op("}")
    )
  
  // ASN1D 15.7.2<24>
  def atNotation = op("@") ~ componentIdList | op("@.") ~ componentIdList

  // ASN1D 15.7.2<28>
  def componentIdList = rep1sep(identifier, op("."))

  // ASN1D 15.7.2<36>
  def typeConstraint = `type`

  // ASN1D 15.9<6>
  def instanceOfValue = value
  
  // ASN1D 15.9.2<1>
  def usefulObjectClassReference = kw("TYPE-IDENTIFIER") | kw("ABSTRACT-SYNTAX")
  
  // ASN1D 15.9.2<2>
  def instanceOfType =
    ( kw("INSTANCE") ~ kw("OF") ~ definedObjectClass
    ) ^^ { _ => InstanceOfType() } // TODO

  // ASN1D 15.10.2<1>
  // See ASN1D 15.9.2<1>

  // ASN1D 17.2.2<1>
  def parameterizedAssignment =
    ( parameterizedTypeAssignment
    | parameterizedValueAssignment
    | parameterizedValueSetTypeAssignment
    | parameterizedObjectClassAssignment
    | parameterizedObjectAssignment
    | parameterizedObjectSetAssignment
    ) ^^ { _ => ParameterizedAssignment() } // TODO
  
  // ASN1D 17.2.2<3>
  def parameterizedTypeAssignment = typeReference ~ parameterList ~ op("::=") ~ `type`

  // ASN1D 17.2.2<5>
  def parameterizedValueAssignment = valueReference ~ parameterList ~ `type` ~ op("::=") ~ value

  // ASN1D 17.2.2<6>
  def parameterizedValueSetTypeAssignment = typeReference ~ parameterList ~ `type` ~ op("::=") ~ valueSet

  // ASN1D 17.2.2<8>
  def parameterizedObjectClassAssignment = objectClassReference ~ parameterList ~ op("::=") ~ objectClass

  // ASN1D 17.2.2<9>
  def parameterizedObjectAssignment =
    ( objectReference
    ~ parameterList
    ~ definedObjectClass
    ~ op("::=")
    ~ object_
    )

  // ASN1D 17.2.2<10>
  def parameterizedObjectSetAssignment =
    ( objectSetReference
    ~ parameterList
    ~ definedObjectClass
    ~ op("::=")
    ~ objectSet
    )

  // ASN1D 17.2.2<11>
  def parameterList = op("{") ~ rep1sep(parameter, op(",")) ~ op("}")
  def parameter = paramGovernor ~ op(":") ~ dummyReference | dummyReference
  
  // ASN1D 17.2.2<12>
  def paramGovernor = governor | dummyGovernor
  // Se ASN1D 13.13.2<7>
  def dummyGovernor = dummyReference
  def dummyReference = reference
  
  // ASN1D 17.2.2<25>
  def parameterizedType = simpleDefinedType ~ actualParameterList
  def parameterizedValueSetType = simpleDefinedType ~ actualParameterList
  def simpleDefinedType = externalTypeReference | typeReference
  
  // ASN1D 17.2.2<27>
  def parameterizedValue = simpleDefinedValue ~ actualParameterList
  def simpleDefinedValue = externalValueReference | valueReference
  
  // ASN1D 17.2.2<29>
  def parameterizedObjectClass = definedObjectClass ~ actualParameterList

  // ASN1D 17.2.2<30>
  def parameterizedObject = definedObject ~ actualParameterList

  // ASN1D 17.2.2<31>
  def parameterizedObjectSet = definedObjectSet ~ actualParameterList

  // ASN1D 17.2.2<32>
  def actualParameterList = op("{") ~ rep1sep(actualParameter, op(",")) ~ op("}")
  def actualParameter : Parser[Any] = `type` | value | valueSet | definedObjectClass | object_ | objectSet
}

