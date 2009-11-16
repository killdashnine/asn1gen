package org.asn1gen.parsing.asn1

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

import org.asn1gen.parsing.asn1.ast._
import org.asn1gen.parsing.syntax._

class Asn1Parser extends TokenParsers with ImplicitConversions {
  type Tokens = Asn1Lexer
  
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
    , "SEQUENCE", "SET", "SIZE", "STRING", "SYNTAX"
    , "T61String", "TAGS", "TeletexString", "TRUE", "TYPE-IDENTIFIER"
    , "UNION", "UNIQUE", "UNIVERSAL", "UniversalString", "UTCTime", "UTF8String"
    , "VideotexString", "VisibleString"
    , "WITH"
    )
  
  def elem[U](kind: String)(f: PartialFunction[Elem, U]): Parser[U] =
    elem(kind, {_: Elem => true}) ^? (f, _ => "Expecting " + kind + ".")

  def op(chars: String) = elem("operator " + chars) {case lexical.Operator(`chars`) => Operator(chars)}
  def kw(chars: String) = elem("keyword " + chars) {case lexical.Keyword(`chars`) => Keyword(chars)}
  def empty = success("") ^^ { _ => Empty() }

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
    ) ^^ { vfr => ObjectFieldReference(vfr) }
  
  // ASN1D: 8.2.3<21>
  def objectReference =
    ( lexical.Operator("&") ~> valueReference
    ) ^^ { case ValueReference(n) => ObjectReference(n) }

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
  def valueReference = elem("value reference") {
      case lexical.Identifier(n) if (n.first.isLowerCase) => ValueReference(n)
    } | failure ("incorrect type reference")

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
  
  def typeAssignment =
    ( typeReference
    ~ op("::=")
    ~ type_
    ) ^^ { case n ~ _ ~ t => TypeAssignment(n, t) } | failure("type assignment")
  
  def type_ : Parser[Type_] =
    ( builtinType
    | referencedType
    | constrainedType
    ) ^^ { kind => Type_(kind) }
  
  def builtinType =
    ( bitStringType
    | booleanType
    | characterStringType
    | choiceType
    | embeddedPdvType
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
    ) ^^ { kind => ReferencedType(kind) }
  
  // ASN1D 9.1.2<4>
  def valueAssignment =
    ( valueReference
    ~ type_
    ~ op("::=")
    ~ value
    ) ^^ { case vr ~ t ~ _ ~ v => ValueAssignment(vr, t, v) }
  
  // ASN1D 9.1.2<5>
  def value: Parser[Value] =
    ( builtinValue
    | referencedValue
    ) ^^ { kind => Value(kind) }
  
  // ASN1D 9.1.2<6>
  def builtinValue: Parser[BuiltinValue] =
    ( bitStringValue
    | booleanValue
    | characterStringValue
    | choiceValue
    | embeddedPdvValue
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
    ) ^^ { kind => BuiltinValue(kind) }
  
  def referencedValue =
    ( definedValue
    | valueFromObject
    ) ^^ { kind => ReferencedValue(kind) }

  def taggedValue =
    ( value
    ) ^^ { v => TaggedValue(v) }
  
  def valueSetTypeAssignment =
    ( typeReference
    ~ type_
    ~ op("::=")
    ~ valueSet
    ) ^^ { case tr ~ t ~ _ ~ vs => ValueSetTypeAssignment(tr, t, vs) }

  // ASN1D 9.1.2<7-9>
  // Not implemented
    
  // ASN1D 9.1.2<10>
  def objectClassAssignment =
    ( objectClassReference
    ~ op("::=")
    ~ objectClass
    ) ^^ { case ocr ~ _ ~ oc => ObjectClassAssignment(ocr, oc) }
  
  def objectAssignment =
    ( objectReference
    ~ definedObjectClass
    ~ op("::=")
    ~ object_
    ) ^^ { case or ~ doc ~ _ ~ o => ObjectAssignment(or, doc, o) }
  
  def objectSetAssignment =
    ( objectSetReference
    ~ definedObjectClass
    ~ op("::=")
    ~ objectSet
    ) ^^ { case osr ~ doc ~ _ ~ os => ObjectSetAssignment(osr, doc, os) }
  
  def root =
    ( moduleDefinition
    )
  
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
    ) ^^ { case (mi ~ _ ~ td ~ ed ~ _ ~ _ ~ mb ~ _) =>
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
    ).? ^^ {
      case Some(_ ~ doic ~ _) => DefinitiveIdentifier(Some(doic))
      case None => DefinitiveIdentifier(None)
    }
  
  // ASN1D 9.2.2<5>
  // Not implemented
    
  // ASN1D 9.2.2<6> refactored
  def definitiveObjectIdComponent =
    ( definitiveNameAndNumberForm
    | definitiveNumberForm
    | nameForm
    ) ^^ { kind => DefinitiveObjectIdComponent(kind) }
  
  def definitiveNumberForm =
    ( number
    ) ^^ { n => DefinitiveNumberForm(n) }

  def nameForm =
    ( identifier
    ) ^^ { i => NameForm(i) }
  
  def definitiveNameAndNumberForm =
    ( identifier
    ~ op("(")
    ~ definitiveNumberForm
    ~ op(")")
    ) ^^ { case (i ~ _ ~ dnf ~ _) => DefinitiveNameAndNumberForm(i, dnf) }

  // ASN1D 9.2.2<7>
  // Not implemented
  
  // ASN1D 9.2.2<8>
  def extensionDefault =
    ( ( kw("EXTENSIBILITY")
      ~ kw("IMPLIED")
      ) ^^ { _ => ExtensionDefault(true) }
    | ( empty
      ) ^^ { _ => ExtensionDefault(false) }
    )
  
  // ASN1D 9.2.2<8-11>
  // Not implemented
    
  // ASN1D 9.2.2<12>
  def moduleBody: Parser[ModuleBody] =
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
    ).? ^^ {
      case Some(_ ~ se ~ _) => Exports(Some(se))
      case _ => Exports(None)
    }
  
  def symbolsExported =
    ( repsep(symbol, op(","))
    ) ^^ { ss => SymbolsExported(ss) }

  // ASN1D 9.2.2<13-15>
  // Not implemented
    
  // ASN1D 9.2.2<16>
  def imports =
    ( kw("IMPORTS")
    ~ symbolsImported
    ~ op(";")
    ).? ^^ {
      case Some(_ ~ si ~ _) => Imports(Some(si))
      case _ => Imports(None)
    }

  def symbolsImported =
    ( symbolsFromModule.*
    ) ^^ { sfms => SymbolsImported(sfms) }
  
  def symbolsFromModule =
    ( rep1sep(symbol, op(","))
    ~ kw("FROM")
    ~ globalModuleReference
    ) ^^ { case ss ~ _ ~ gmr => SymbolsFromModule(ss, gmr) }
  
  // ASN1D 9.2.2<22>
  def globalModuleReference =
    ( moduleReference
    ~ assignedIdentifier
    ) ^^ { case mr ~ ai => GlobalModuleReference(mr, ai) }
  
  // ASN1D 9.2.2<27>
  def assignedIdentifier =
    ( objectIdentifierValue
    | definedValue
    | empty
    ) ^^ { kind => AssignedIdentifier(kind) }

  // ASN1D 9.2.2<30> // refactored
  def symbol =
    ( parameterizedReference
    | reference
    ) ^^ { kind => Symbol(kind) }

  def reference =
    ( typeReference
    | valueReference
    | objectClassReference
    | objectReference
    | objectSetReference
    ) ^^ { kind => Reference(kind) }
  
  def parameterizedReference =
    ( reference
    ~ (op("{") ~ op("}")).?
    ) ^^ { case r ~ b => ParameterizedReference(r, b.isDefined) }
  
  // ASN1D 9.3.2<1> refactored
  def definedType =
    ( parameterizedType
    | parameterizedValueSetType
    | externalTypeReference
    | typeReference
    ) ^^ { kind => DefinedType(kind) }

  // ASN1D 9.3.2<3>
  def externalTypeReference =
    ( moduleReference
    ~ op(".")
    ~ typeReference
    ) ^^ { case mr ~ _ ~ tr => ExternalTypeReference(mr, tr) }
  
  // ASN1D 9.3.2<8>
  def definedValue =
    ( externalValueReference
    | valueReference
    | parameterizedValue
    ) ^^ { kind => DefinedValue(kind) }
  
  // ASN1D 9.3.2<10>
  def externalValueReference =
    ( moduleReference
    ~ op(".")
    ~ valueReference
    ) ^^ { case mr ~ _ ~ vr => ExternalValueReference(mr, vr) }
  
  // ASN1D 9.3.2<14>
  def definedObjectClass =
    ( externalObjectClassReference
    | objectClassReference
    | usefulObjectClassReference
    ) ^^ { kind => DefinedObjectClass(kind) }
  
  // ASN1D 9.3.2<15>
  def externalObjectClassReference =
    ( moduleReference
    ~ op(".")
    ~ objectClassReference
    ) ^^ { case mr ~ _ ~ ocr => ExternalObjectClassReference(mr, ocr) }
  
  // ASN1D 9.3.2<19>
  def definedObject =
    ( externalObjectReference
    | objectReference
    ) ^^ { kind => DefinedObject(kind) }
  
  // ASN1D 9.3.2<20>
  def externalObjectReference =
    ( moduleReference
    ~ op(".")
    ~ objectReference
    ) ^^ { case mr ~ _ ~ or => ExternalObjectReference(mr, or) }
  
  // ASN1D 9.3.2<24>
  def definedObjectSet =
    ( externalObjectSetReference
    | objectSetReference
    ) ^^ { kind => DefinedObjectSet(kind) }
  
  def externalObjectSetReference =
    ( moduleReference
    ~ op(".")
    ~ objectSetReference
    ) ^^ { case mr ~ _ ~ osr => ExternalObjectSetReference(mr, osr) }
  
  // ASN1D 10.1.2
  def booleanType =
    ( kw("BOOLEAN")
    ) ^^ { _ => BooleanType() }
  
  def booleanValue =
    ( kw("TRUE") ^^ { _ => BooleanValue(true) }
    | kw("FALSE") ^^ { _ => BooleanValue(false) }
    )
  
  // ASN1D 10.2.2
  def nullType =
    ( kw("NULL")
    ) ^^ { _ => NullType() }
  
  def nullValue =
    ( kw("NULL")
    ) ^^ { _ => NullValue() }
  
  // ASN1D 10.3.2<1>
  def integerType =
    ( kw("INTEGER")
    ~ ( ( op("{")
        ~ rep1sep(namedNumber, op(","))
        ~ op("}")
        ) ^^ { case _ ~ nns ~ _ => nns }
      ).?
    ) ^^ { case _ ~ nns => IntegerType(nns) }
  
  // ASN1D 10.3.2<6>
  def namedNumber =
    ( identifier
    ~ op("(")
    ~ (signedNumber | definedValue)
    ~ op(")")
    ) ^^ { case id ~ _ ~ value ~ _ => NamedNumber(id, value) }
  
  // ASN1D 10.3.2<11>
  def integerValue =
    ( signedNumber
    | identifier
    ) ^^ { kind => IntegerValue(kind) }
  
  // ASN1D 10.4.2<1>
  def enumeratedType =
    ( kw("ENUMERATED")
    ~ op("{")
    ~ enumerations
    ~ op("}")
    ) ^^ { case _ ~ _ ~ e ~ _ => EnumeratedType(e) }
  
  // ASN1D 10.4.2<5>
  def enumerationsExtension2 =
    ( op(",")
    ~ additionalEnumeration
    ) ^^ { case _ ~ ae => EnumerationsExtension2(ae) }
      
  def enumerationsExtension1 =
    ( op(",")
    ~ op("...")
    ~ exceptionSpec
    ~ enumerationsExtension2.?
    ) ^^ { case _ ~ _ ~ es ~ ee2 => EnumerationsExtension1(es, ee2) }
    
  def enumerations =
    ( rootEnumeration
    ~ enumerationsExtension1.?
    ) ^^ { case re ~ ee1 => Enumerations(re, ee1) }
  
  // ASN1D 10.4.2<7>
  def rootEnumeration =
    ( enumeration
    ) ^^ { e => RootEnumeration(e) }

  // ASN1D 10.4.2<8>
  def additionalEnumeration =
    ( enumeration
    ) ^^ { e => AdditionalEnumeration(e) }

  // ASN1D 10.4.2<11>
  def enumeration =
    ( rep1sep(enumerationItem, op(","))
    ) ^^ { eis => Enumeration(eis) }
  
  def enumerationItem =
    ( identifier
    | namedNumber
    ) ^^ { kind => EnumerationItem(kind) }
  
  // ASN1D 10.4.2<13>
  // See 10.3.2<6>
  // Not implemented
  
  // ASN1D 10.4.2<16>
  def enumeratedValue =
    ( identifier
    ) ^^ { i => EnumeratedValue(i) }
  
  // ASN1D 10.5.2<1>
  def realType =
    ( kw("REAL")
    ) ^^ { _ => RealType() }
  
  // ASN1D 10.5.2<5>
  def realValue =
    ( numericRealValue
    | specialRealValue
    ) ^^ { kind => RealValue(kind) }
  
  def numericRealValue =
    ( number ^? { case Number("0") => Number("0") }
    | sequenceValue
    ) ^^ { kind => NumericRealValue(kind) }
  
  // ASN1D 10.5.2<8>
  def specialRealValue =
    ( kw("PLUS-INFINITY") ^^ { _ => SpecialRealValue(true) }
    | kw("MINUS-INFINITY") ^^ { _ => SpecialRealValue(false) }
    )
  
  // ASN1D 10.6.2<1>
  def bitStringType =
    ( kw("BIT")
    ~ kw("STRING")
    ~ ( ( op("{")
        ~ rep1sep(namedBit, op(","))
        ~ op("}")
        ) ^^ { case _ ~ nbs ~ _ => nbs }
      ).?
    ) ^^ { case _ ~ _ ~ nbs => BitStringType(nbs) }
  
  // ASN1D 10.6.2<6>
  def namedBit =
    ( identifier
    ~ op("(")
    ~ ( number
      | definedValue
      )
    ~ op(")")
    ) ^^ { case i ~ _ ~ kind ~ _ => NamedBit(i, kind) }
  
  // ASN1D 10.6.2<13>
  def bitStringValue =
    ( bstring
    | hstring
    | identifierList
    ) ^^ { kind => BitStringValue(kind) }
  
  // ASN1D 10.6.2<16>
  def identifierList =
    ( op("{")
    ~ repsep(identifier, op(","))
    ~ op("}")
    ) ^^ { case _ ~ is ~ _ => IdentifierList(is) }
  
  // ASN1D 10.7.2<1>
  def octetStringType =
    ( kw("OCTET")
    ~ kw("STRING")
    ) ^^ { _ => OctetStringType() }
  
  // ASN1D 10.7.2<4>
  def octetStringValue =
    ( bstring
    | hstring
    ) ^^ { kind => OctetStringValue(kind) }
  
  // ASN1D 10.8.2<1>
  def objectIdentifierType =
    ( kw("OBJECT")
    ~ kw("IDENTIFIER")
    ) ^^ { _ => ObjectIdentifierType() }
  
  // ASN1D 10.8.2<3>
  def objectIdentifierValue =
    ( op("{")
    ~ ( definedValue.?
      ~ objIdComponents.+
      )
    ~ op("}")
    ) ^^ { _ => ObjectIdentifierValue() }
  
  // ASN1D 10.8.2<5> refactored
  def objIdComponents =
    ( nameAndNumberForm
    | nameForm
    | numberForm
    | definedValue
    ) ^^ { kind => ObjIdComponents(kind) }
  
  // ASN1D 10.8.2<6>
  // See ASN1D 9.2.2<6>
  
  // ASN1D 10.8.6<8>
  def numberForm =
    ( number
    | definedValue
    ) ^^ { kind => NumberForm(kind) }
  
  // ASN1D 10.8.6<10>
  def nameAndNumberForm =
    ( identifier
    ~ op("(")
    ~ numberForm
    ~ op(")")
    ) ^^ { case i ~ _ ~ n ~ _ => NameAndNumberForm(i, n) }
  
  // ASN1D 10.9.2<2>
  def relativeOidType =
    ( kw("RELATIVE-OID")
    ) ^^ { _ => RelativeOidType() } // TODO
  
  // ASN1D 10.9.2<4>
  def relativeOidValue =
    ( op("{")
    ~ relativeOidComponents.+
    ~ op("}")
    ) ^^ { case _ ~ rocs ~ _ => RelativeOidValue(rocs) }
  
  def relativeOidComponents =
    ( numberForm
    | nameAndNumberForm
    | definedValue
    ) ^^ { value => RelativeOidComponents(value) }
  
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
    ) ^^ { kind => RestrictedCharacterStringValue(kind) }
  
  def characterStringList =
    ( op("{")
    ~ rep1sep(charsDefn, op(","))
    ~ op("}")
    ) ^^ { case _ ~ cds ~ _ => CharacterStringList(cds) }
  
  def charsDefn =
    ( cstring
    | quadruple
    | tuple
    | definedValue
    ) ^^ { kind => CharsDefn(kind) }
  
  // ASN1D 11.10.2<9>
  def quadruple =
    ( op("{") ~ group
    ~ op(",") ~ plane
    ~ op(",") ~ row
    ~ op(",") ~ cell
    ~ op("}")
    ) ^^ { case _ ~ g ~ _ ~ p ~ _ ~ r ~ _ ~ c ~ _ => Quadruple(g, p, r, c) }

  def group =
    ( number
    ) ^^ { n => Group(n) }
  def plane =
    ( number
    ) ^^ { n => Plane(n) }
  def row =
    ( number
    ) ^^ { n => Row(n) }
  def cell =
    ( number
    ) ^^ { n => Cell(n) }
  
  // ASN1D 11.13<1>
  def characterStringType =
    ( restrictedCharacterStringType
    | unrestrictedCharacterStringType
    ) ^^ { kind => CharacterStringType(kind) }

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
    ) ^^ { case Keyword(kw) => RestrictedCharacterStringType(kw) }

  // ASN1D 11.13<36>
  def characterStringValue =
    ( restrictedCharacterStringValue
    | unrestrictedCharacterStringValue
    ) ^^ { kind => CharacterStringValue(kind) }
  
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
    ) ^^ { case _ ~ tc ~ _ ~ tr ~ _ => Tuple(tc, tr) }
  
  def tableColumn =
    ( number
    ) ^^ { n => TableColumn(n) }
  
  def tableRow =
    ( number
    ) ^^ { n => TableRow(n) }
  
  // ASN1D 11.15.2
  def usefulType =
    ( kw("GeneralizedTime")
    | kw("UTCTime")
    | kw("ObjectDescriptor")
    ) ^^ { case Keyword(kw) => UsefulType(kw) }
  
  // ASN1D 11.16.2<1>
  // See ASN1D 11.15.2<1>
  
  // ASN1D 11.17.2<1>
  // See ASN1D 11.15.2<1>

  // ASN1D 12.1.4<1>
  def taggedType =
    ( ( tag
      ~ type_
      ) ^^ { case tag ~ t => DefaultTaggedType(tag, t) }
    | ( tag
      ~ kw("IMPLICIT")
      ~ type_
      ) ^^ { case tag ~ _ ~ t => ImplicitTaggedType(tag, t) }
    | ( tag
      ~ kw("EXPLICIT")
      ~ type_
      ) ^^ { case tag ~ _ ~ t => ExplicitTaggedType(tag, t) }
    )
  
  def tag =
    ( op("[")
    ~ class_
    ~ classNumber
    ~ op("]")
    ) ^^ { case _ ~ c ~ cn ~ _ => Tag(c, cn) }
  
  def classNumber =
    ( number
    | definedValue
    ) ^^ { kind => ClassNumber(kind) }
  
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
      ) ^^ { _ => EmptySequenceType() }
    | ( kw("SEQUENCE")
      ~ op("{")
      ~ extensionAndException
      ~ optionalExtensionMarker
      ~ op("}")
      ) ^^ { _ => ExtensionSequenceType() } // TODO
    | ( kw("SEQUENCE")
      ~ op("{")
      ~ componentTypeLists
      ~ op("}")
      ) ^^ { case _ ~ _ ~ ctl ~ _ => ComponentSequenceType(ctl) }
    ) ^^ { _ => SequenceType() }
  
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
    ) ^^ { _ => ComponentTypeLists() }
  
  // ASN1D 12.2.2<5>
  def rootComponentTypeList =
    ( componentTypeList
    ) ^^ { ctl => RootComponentTypeList(ctl) }
  
  def extensionsAdditions =
    ( ( op(",")
      ~ extensionAdditionList
      )
    | empty
    ) ^^ { _ => ExtensionsAdditions() }

  def extensionAdditionList =
    ( rep1sep(extensionAddition, op(","))
    ) ^^ { eas => ExtensionAdditionList(eas) }
  def extensionAddition =
    ( componentType ~ extensionAdditionGroup
    ) ^^ { case ct ~ eag => ExtensionAddition(ct, eag) }
  def extensionAdditionGroup =
    ( op("[[") ~ componentTypeList ~ op("]]")
    ) ^^ { case _ ~ ctl ~ _ => ExtensionAdditionGroup(ctl) }
  def componentTypeList =
    ( rep1sep(componentType, op(","))
    ) ^^ { cts => ComponentTypeList(cts) }
  
  // ASN1D 12.2.2<13> refactored
  def componentType =
    ( namedType
    ~ ( kw("OPTIONAL")
      | kw("DEFAULT") ~ value
      ).?
    | kw("COMPONENTS") ~ kw("OF") ~ type_
    ) ^^ { _ => ComponentType() }
  
  // ASN1D 12.2.2<24>
  def namedType =
    ( identifier ~ type_
    ) ^^ { case id ~ t => NamedType(id, t) }

  // ASN1D 12.2.2<25>
  def sequenceValue =
    ( op("{")
    ~ repsep(namedValue, op(","))
    ~ op("}")
    ) ^^ { case _ ~ nvs ~ _ => SequenceValue(nvs) }

  // ASN1D 12.2.2<31>
  def namedValue =
    ( identifier ~ value
    ) ^^ { case i ~ v => NamedValue(i, v) }

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
    ) ^^ { case _ ~ nvs ~ _ => SetValue(nvs) }

  // ASN1D 12.3.2<31>
  // See ASN1D 12.2.2<31>

  // ASN1D 12.4.2<1>
  def sequenceOfType =
    ( kw("SEQUENCE") ~ kw("OF") ~ type_
    ) ^^ { case _ ~ _ ~ t => SequenceOfType(t) } // TODO

  // ASN1D 12.4.2<3>
  def typeWithConstraint =
    ( kw("SEQUENCE") ~ constraint ~ kw("OF") ~ type_
    | kw("SEQUENCE") ~ sizeConstraint ~ kw("OF") ~ type_
    | kw("SET") ~ constraint ~ kw("OF") ~ type_
    | kw("SET") ~ sizeConstraint ~ kw("OF") ~ type_
    ) ^^ { _ => TypeWithConstraint() }
  
  // ASN1D 12.4.2<5>
  def sizeConstraint =
    ( kw("SIZE") ~ constraint
    ) ^^ { case (_ ~ c) => SizeConstraint(c) }

  // ASN1D 12.4.2<8>
  def sequenceOfValue =
    ( op("{") ~ repsep(value, op(",")) ~ op("}")
    ) ^^ { case (_ ~ vs ~ _) => SequenceOfValue(vs) }

  // ASN1D 12.5.2<1>
  def setOfType =
    ( kw("SET") ~ kw("OF") ~ type_
    ) ^^ { case (_ ~ _ ~ t) => SetOfType(t) } // TODO

  // ASN1D 12.5.2<3>
  // See ASN1D 12.4.2<3>
  
  // ASN1D 12.5.2<5>
  // See ASN1D 12.4.2<5>

  // ASN1D 12.5.2<8>
  def setOfValue =
    ( op("{") ~ repsep(value, op(",")) ~ op("}")
    ) ^^ { case _ ~ v ~ _ => SetOfValue(v) }

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
    ( alternativeTypeList
    ) ^^ { atl => RootAlternativeTypeList(atl) }
  def extensionAdditionAlternatives =
    ( op(",") ~ extensionAdditionAlternativesList
    | empty
    ) ^^ { _ => ExtensionAdditionAlternatives() } // TODO

  def extensionAdditionAlternativesList =
    ( rep1sep(extensionAdditionAlternative, op(","))
    ) ^^ { eaas => ExtensionAdditionAlternativesList(eaas) }

  def extensionAdditionAlternative =
    ( extensionAdditionGroupAlternatives
    | namedType
    ) ^^ { kind => ExtensionAdditionAlternative(kind) }

  def extensionAdditionGroupAlternatives =
    ( op("[[")
    ~ alternativeTypeList
    ~ op("]]")
    ) ^^ { case _ ~ atl ~ _ => ExtensionAdditionGroupAlternatives(atl) }

  def alternativeTypeList =
    ( rep1sep(namedType, op(","))
    ) ^^ { namedTypes => AlternativeTypeList(namedTypes) }
  
  // ASN1D 12.6.1<10>
  // See ASN1D 12.2.2<24>

  // ASN1D 12.6.1<14>
  def choiceValue =
    ( identifier ~ op(":") ~ value
    ) ^^ { case i ~ _ ~ v => ChoiceValue(i, v) }

  // ASN1D 12.7.2<1>
  def selectionType =
    ( identifier ~ op("<") ~ type_
    ) ^^ { case i ~ _ ~ t => SelectionType(i, t) }

  // ASN1D 12.9.2<1>
  def extensionAndException =
    ( op("...")
    | op("...") ~ exceptionSpec
    ) ^^ { _ => ExtensionAndException() } // TODO

  def optionalExtensionMarker =
    ( op(",") ~ op("...")
    | empty
    ) ^^ { _ => OptionalExtensionMarker() } // TODO

  def extensionEndMarker =
    ( op(",") ~ op("...")
    ) ^^ { _ => ExtensionEndMarker() }
  
  // ASN1D 12.9.2<18>
  def exceptionSpec =
    ( op("!") ~ exceptionIdentification
    | empty
    ) ^^ { _ => ExceptionSpec() }

  // ASN1D 12.9.2<20>
  def exceptionIdentification =
    ( signedNumber
    | definedValue
    | type_ ~ op(":") ~ value
    ) ^^ { _ => ExceptionIdentification() }
  
  // ASN1D 13.1.2<1>
  def constrainedType =
    ( type_ ~ constraint
    | typeWithConstraint
    ) ^^ { _ => ConstrainedType() } // TODO

  // ASN1D 13.2.2<1>
  def singleValue =
    ( value
    ) ^^ { v => SingleValue(v) }

  // ASN1D 13.3.2<1>
  def containedSubtype =
    ( includes ~ type_
    ) ^^ { case i ~ t => ContainedSubtype(i, t) }

  // ASN1D 13.3.7<7>
  def includes =
    ( kw("INCLUDES")
    | empty
    ) ^^ { _ => Includes() }

  // ASN1D 13.4.2<1>
  def valueRange =
    ( lowerEndPoint ~ op("..") ~ upperEndPoint
    ) ^^ { case lep ~ _ ~ uep => ValueRange(lep, uep) }

  // ASN1D 13.4.2<3>
  def lowerEndPoint =
    ( lowerEndValue | lowerEndValue ~ op("<")
    ) ^^ { _ => LowerEndPoint() }
  def upperEndPoint =
    ( upperEndValue | op("<") ~ upperEndValue
    ) ^^ { _ => UpperEndPoint() }
  
  // ASN1D 13.4.2<4>
  def lowerEndValue =
    ( value | kw("MIN")
    ) ^^ { _ => LowerEndValue() }
  def upperEndValue =
    ( value | kw("MAX")
    ) ^^ { _ => UpperEndValue() }
  
  // ASN1D 13.5.2<1>
  // See ASN1D 12.4.2<5>

  // ASN1D 13.5.2<5>
  // See ASN1D 12.4.2<3>
  
  // ASN1D 13.6.2<1>
  def permittedAlphabet =
    ( kw("FROM") ~ constraint
    ) ^^ { case _ ~ c => PermittedAlphabet(c) }
  
  // ASN1D 13.6.2<5>
  // See ASN1D 13.4.2<1>

  // ASN1D 13.6.2<9>
  // See ASN1D 13.4.2<3>
  
  // ASN1D 13.6.2<10>
  // See ASN1D 13.4.2<4>
  
  // ASN1D 13.7.2<1>
  def patternConstraint =
    ( kw("PATTERN") ~ value
    ) ^^ { case _ ~ v => PatternConstraint(v) }

  // ASN1D 13.8.2<1>
  def innerTypeConstraints =
    ( kw("WITH") ~ kw("COMPONENT") ~ singleTypeConstraint
    | kw("WITH") ~ kw("COMPONENTS") ~ multipleTypeConstraints
    ) ^^ { _ => InnerTypeConstraints() }
  
  // ASN1D 13.8.2<3>
  def singleTypeConstraint =
    ( constraint
    ) ^^ { c => SingleTypeConstraint(c) }

  // ASN1D 13.9.2<1>
  // See ASN1D 13.8.2<1>
  
  // ASN1D 13.9.2<4>
  def multipleTypeConstraints =
    ( fullSpecification
    | partialSpecification
    ) ^^ { kind => MultipleTypeConstraints(kind) }

  // ASN1D 13.9.2<5>
  def fullSpecification =
    ( op("{") ~ typeConstraints ~ op("}")
    ) ^^ { case _ ~ tc ~ _ => FullSpecification(tc) }

  // ASN1D 13.9.2<7>
  def partialSpecification =
    ( op("{")
    ~ op("...")
    ~ op(",")
    ~ typeConstraints
    ~ op("}")
    ) ^^ { case _ ~ _ ~ _ ~ tc ~ _ => PartialSpecification(tc) }

  // ASN1D 13.9.2<9>
  def typeConstraints =
    ( rep1sep(namedConstraint, op(","))
    ) ^^ { namedConstraints => TypeConstraints(namedConstraints) }

  // ASN1D 13.9.2<10>
  def namedConstraint =
    ( identifier
    ~ componentConstraint
    ) ^^ { case i ~ cc => NamedConstraint(i, cc) }

  // ASN1D 13.9.2<12>
  def componentConstraint =
    ( valueConstraint
    ~ presenceConstraint
    ) ^^ { case vc ~ pc => ComponentConstraint(vc, pc) }
  def valueConstraint =
    ( constraint
    | empty
    ) ^^ { kind => ValueConstraint(kind) }
  
  // ASN1D 13.9.2<14>
  def presenceConstraint =
    ( kw("PRESENT")
    | kw("ABSENT")
    | kw("OPTIONAL")
    | empty
    ) ^^ { _ => PresenceConstraint() }
  
  // ASN1D 13.10.2<1>
  def contentsConstraint =
    ( kw("CONTAINING") ~ type_
    | kw("ENCODED") ~ kw("BY") ~ value
    | kw("CONTAINING") ~ type_ ~ kw("ENCODED") ~ kw("BY") ~ value
    ) ^^ { _ => ContentsConstraint() }
  
  // ASN1D 13.11.2<1>
  def elementSetSpecs: Parser[ElementSetSpecs] =
    ( rootElementSetSpec
    | rootElementSetSpec ~ op(",") ~ op("...")
    | rootElementSetSpec ~ op(",") ~ op("...") ~ op(",") ~ additionalElementSetSpec
    ) ^^ { _ => ElementSetSpecs() }
  
  // ASN1D 13.11.2<2>
  def rootElementSetSpec =
    ( elementSetSpec
    ) ^^ { ess => RootElementSetSpec(ess) }
  def additionalElementSetSpec =
    ( elementSetSpec
    ) ^^ { ess => AdditionalElementSetSpec(ess) }
  
  // ASN1D 13.11.2<9>
  def elementSetSpec: Parser[ElementSetSpec] =
    ( unions
    | kw("ALL") ~ exclusions
    ) ^^ { _ => ElementSetSpec() }

  // ASN1D 13.11.2<10>
  def unions: Parser[Unions] =
    ( intersections
    | uElems ~ unionMark ~ intersections
    ) ^^ { _ => Unions() }

  // ASN1D 13.11.2<11>
  def uElems =
    ( unions
    ) ^^ { _ => UElems() }
  def unionMark =
    ( op("|")
    | kw("UNION")
    ) ^^ { _ => UnionMark() }
 
  // ASN1D 13.11.2<12>
  def intersections: Parser[Intersections] =
    ( intersectionElements
    | iElems ~ intersectionMark ~ intersectionElements
    ) ^^ { _ => Intersections() }
  
  // ASN1D 13.11.2<13>
  def iElems =
    ( intersections
    ) ^^ { _ => IElems() }
  def intersectionMark =
    ( op("^")
    | kw("INTERSECTION")
    ) ^^ { _ => IntersectionMark() }
  def intersectionElements =
    ( elements
    | elems ~ exclusions
    ) ^^ { _ => IntersectionElements() }
  
  // ASN1D 13.11.2<14>
  def elems =
    ( elements
    ) ^^ { _ => Elems() }
  def exclusions =
    ( kw("EXCEPT") ~ elements
    ) ^^ { case _ ~ e => Exclusions(e) }

  // ASN1D 13.11.2<16>
  def elements =
    ( subtypeElements
    | objectSetElements
    | op("(") ~ elementSetSpec ~ op(")")
    ) ^^ { _ => Elements() }
  
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
    ) ^^ { kind => SubtypeElements(kind) }
  
  // ASN1D 13.12.2<1>
  def constraint =
    ( op("(") ~ constraintSpec ~ exceptionSpec ~ op(")")
    ) ^^ { case _ ~ cs ~ es ~ _ => Constraint(cs, es) }

  // ASN1D 13.12.2<5>
  def constraintSpec =
    ( elementSetSpecs
    | generalConstraint
    ) ^^ { kind => ConstraintSpec(kind) }

  // ASN1D 13.12.2<6>
  // See ASN1D 13.11.2<1>

  // ASN1D 13.13.2<1>
  def userDefinedConstraint =
    ( kw("CONSTRAINED")
    ~ kw("BY")
    ~ op("{")
    ~ repsep(userDefinedConstraintParameter, op(","))
    ~ op("}")
    ) ^^ { case _ ~ _ ~ _ ~ parameters ~ _ => UserDefinedConstraint(parameters) }
  
  // ASN1D 13.13.2<3>
  def userDefinedConstraintParameter	=
    ( governor ~ op(":") ~ value
    | governor ~ op(":") ~ valueSet
    | governor ~ op(":") ~ object_
    | governor ~ op(":") ~ objectSet
    | type_
    | definedObjectClass
    ) ^^ { _ => UserDefinedConstraintParameter() }
  
  // ASN1D 13.13.2<7>
  def governor =
    ( type_
    | definedObjectClass
    ) ^^ { kind => Governor(kind) }

  // ASN1D 14.1.2<1>
  def externalType =
    ( kw("EXTERNAL")
    ) ^^ { _ => ExternalType() } // TODO

  // ASN1D 14.1.2<7>
  def externalValue =
    ( sequenceValue
    ) ^^ { sv => ExternalValue(sv) }

  // ASN1D 14.2.2<1>
  def embeddedPdvType =
    ( kw("EMBEDDED") ~ kw("PDV")
    ) ^^ { _ => EmbeddedPdvType() }

  // ASN1D 14.2.2<5>
  def embeddedPdvValue =
    ( sequenceValue
    ) ^^ { sv => EmbeddedPdvValue(sv) }

  // ASN1D 14.3.2<1>
  def unrestrictedCharacterStringType =
    ( kw("CHARACTER") ~ kw("STRING")
    ) ^^ { _ => UnrestrictedCharacterStringType() }

  // ASN1D 14.3.2<6>
  def unrestrictedCharacterStringValue =
    ( sequenceValue
    ) ^^ { sv => UnrestrictedCharacterStringValue(sv) }

  // ASN1D 15.2.2<1>
  def objectClass =
    ( definedObjectClass
    | objectClassDefn
    | parameterizedObjectClass
    ) ^^ { kind => ObjectClass(kind) }
  def objectClassDefn =
    ( kw("CLASS")
    ~ op("{")
    ~ rep1sep(fieldSpec, op(","))
    ~ withSyntaxSpec
    ) ^^ { case _ ~ _ ~ fieldSpecs ~ wss => ObjectClassDefn(fieldSpecs, wss) }

  def fieldSpec =
    ( typeFieldSpec
    | fixedTypeValueFieldSpec
    | variableTypeValueFieldSpec
    | fixedTypeValueSetFieldSpec
    | variableTypeValueSetFieldSpec
    | objectFieldSpec
    | objectSetFieldSpec
    ) ^^ { kind => FieldSpec(kind) }
  
  // ASN1D 15.2.2<3>
  def typeFieldSpec =
    ( typeFieldReference ~ typeOptionalitySpec
    ) ^^ { case tfr ~ tos => TypeFieldSpec(tfr, tos) }
  def typeOptionalitySpec =
    ( kw("OPTIONAL")
    | kw("DEFAULT") ~ type_
    | empty
    ) ^^ { _ => TypeOptionalitySpec() }
  
  // ASN1D 15.2.2<6>
  def fixedTypeValueFieldSpec =
    ( valueFieldReference
    ~ type_
    ~ unique
    ~ valueOptionalitySpec
    ) ^^ { case vfr ~ t ~ u ~ vos => FixedTypeValueFieldSpec(vfr, t, u, vos) }

  // ASN1D 15.2.2<7>
  def unique =
    ( kw("UNIQUE")
    | empty
    ) ^^ { _ => Unique() }

  // ASN1D 15.2.2<10>
  def valueOptionalitySpec =
    ( kw("OPTIONAL")
    | kw("DEFAULT") ~ value
    | empty
    ) ^^ { _ => ValueOptionalitySpec() }
  
  // ASN1D 15.2.2<13>
  def variableTypeValueFieldSpec =
    ( valueFieldReference ~ fieldName ~ valueOptionalitySpec
    ) ^^ { case vfr ~ fn ~ vos => VariableTypeValueFieldSpec(vfr, fn, vos) }

  // ASN1D 15.2.2<17>
  def fixedTypeValueSetFieldSpec =
    ( valueSetFieldReference ~ type_ ~ valueSetOptionalitySpec
    ) ^^ { case vsfr ~ t ~ vsos => FixedTypeValueSetFieldSpec(vsfr, t, vsos) }

  // ASN1D 15.2.2<18>
  def valueSetOptionalitySpec =
    ( kw("OPTIONAL")
    | kw("DEFAULT") ~ valueSet
    | empty
    ) ^^ { _ => ValueSetOptionalitySpec() }
  
  // ASN1D 15.2.2<21>
  def variableTypeValueSetFieldSpec =
    ( valueSetFieldReference ~ fieldName ~ valueSetOptionalitySpec
    ) ^^ { case vsfr ~ fn ~ vsos => VariableTypeValueSetFieldSpec(vsfr, fn, vsos) }

  // ASN1D 15.2.2<25>
  def objectFieldSpec =
    ( objectFieldReference ~ definedObjectClass ~ objectOptionalitySpec
    ) ^^ { case ofr ~ doc ~ oos => ObjectFieldSpec(ofr, doc, oos) }

  // ASN1D 15.2.2<26>
  def objectOptionalitySpec =
    ( kw("OPTIONAL")
    | kw("DEFAULT") ~ object_
    | empty
    ) ^^ { _ => ObjectOptionalitySpec() }
  
  // ASN1D 15.2.2<28>
  def objectSetFieldSpec =
    ( objectSetFieldReference ~ definedObjectClass ~ objectSetOptionalitySpec
    ) ^^ { case osfr ~ doc ~ osos => ObjectSetFieldSpec(osfr, doc, osos) }
  
  // ASN1D 15.2.2<29>
  def objectSetOptionalitySpec : Parser[ObjectSetOptionalitySpec] =
    ( kw("OPTIONAL") ^^ { _ => OptionalObjectSetOptionalitySpec() }
    | kw("DEFAULT") ~ objectSet ^^ { case _ ~ d => DefaultObjectSetOptionalitySpec(d) }
    | empty ^^ { _ => NoObjectSetOptionalitySpec() }
    )
  
  // ASN1D 15.2.2<33>
  def fieldName =
    ( rep1sep(primitiveFieldName, op("."))
    ) ^^ { pfns => FieldName(pfns) }
  def primitiveFieldName =
    ( typeFieldReference
    | valueFieldReference
    | valueSetFieldReference
    | objectFieldReference
    | objectSetFieldReference
    ) ^^ { value => PrimitiveFieldName(value) }

  // ASN1D 15.2.2<34>
  def object_ : Parser[Object_] =
    ( objectDefn
    | definedObject
    | objectFromObject
    | parameterizedObject
    ) ^^ { kind => Object_(kind) }

  // ASN1D 15.2.2<35>
  def objectDefn =
    ( defaultSyntax
    | definedSyntax
    ) ^^ { kind => ObjectDefn(kind) }

  // ASN1D 15.2.2<36>
  def defaultSyntax =
    ( op("{") ~ repsep(fieldSetting, op(",")) ~ op("}")
    ) ^^ { case _ ~ fss ~ _ => DefaultSyntax(fss) }
  def fieldSetting =
    ( primitiveFieldName ~ setting
    ) ^^ { case pfn ~ s => FieldSetting(pfn, s) }

  // ASN1D 15.2.2<38>
  def setting =
    ( type_
    | value
    | valueSet
    | object_
    | objectSet
    ) ^^ { kind => Setting(kind) }
  
  // ASN1D 15.3.2<1>
  def withSyntaxSpec =
    ( kw("WITH") ~> kw("SYNTAX") ~> syntaxList
    | empty
    ) ^^ { kind => WithSyntaxSpec(kind) }

  // ASN1D 15.3.2<2>
  def syntaxList =
    ( op("{") ~ tokenOrGroupSpec.+ ~ op("}")
    ) ^^ { case _ ~ togss ~ _ => SyntaxList(togss) }
  def tokenOrGroupSpec =
    ( requiredToken
    | optionalGroup
    ) ^^ { kind => TokenOrGroupSpec(kind) }
  def requiredToken =
    ( literal
    | primitiveFieldName
    ) ^^ { kind => RequiredToken(kind) }
  
  // ASN1D 15.3.2<3>
  def optionalGroup: Parser[OptionalGroup] =
    ( op("[") ~ tokenOrGroupSpec.+ ~ op("]")
    ) ^^ { case _ ~ togss ~ _ => OptionalGroup(togss) }

  // ASN1D 15.3.2<8>
  def literal =
    ( word
    | op(",")
    ) ^^ { _ => Literal() }

  // ASN1D 15.3.2<11>
  def definedSyntax =
    ( op("{") ~ definedSyntaxToken.* ~ op("}")
    ) ^^ { case _ ~ dsts ~ _ => DefinedSyntax(dsts) }
  def definedSyntaxToken = literal ^^ { _ => DefinedSyntaxToken() }
  // See ASN1D 15.3.2<8>
  // See ASN1D 15.2.2<38>
  
  // ASN1D 15.5.2<1>
  def objectSet: Parser[ObjectSet] =
    ( op("{") ~ objectSetSpec ~ op("}")
    ) ^^ { case _ ~ oss ~ _ => ObjectSet(oss) }

  // ASN1D 15.5.2<2>
  def objectSetSpec =
    ( rootElementSetSpec
    | rootElementSetSpec ~ op(",") ~ op("...")
    | rootElementSetSpec ~ op(",") ~ op("...") ~ op(",") ~ additionalElementSetSpec
    | op("...")
    | op("...") ~ op(",") ~ additionalElementSetSpec
    ) ^^ { _ => ObjectSetSpec() }
  
  // ASN1D 15.5.2<11>
  def valueSet =
    ( op("{") ~ elementSetSpecs ~ op("}")
    ) ^^ { case _ ~ ess ~ _ => ValueSet(ess) }
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
  def objectSetElements =
    ( object_
    | definedObjectSet
    | objectSetFromObjects
    | parameterizedObjectSet
    ) ^^ { kind => ObjectSetElements(kind) }
  
  // ASN1D 15.6.2<1>
  def valueFromObject =
    ( referencedObjects ~ op(".") ~ fieldName
    ) ^^ { case ro ~ _ ~ fn => ValueFromObject(ro, fn) }

  // ASN1D 15.6.2<5>
  def valueSetFromObjects =
    ( referencedObjects ~ op(".") ~ fieldName
    ) ^^ { case ro ~ _ ~ fn => ValueSetFromObjects(ro, fn) }

  // ASN1D 15.6.2<10>
  def typeFromObject =
    ( referencedObjects ~ op(".") ~ fieldName
    ) ^^ { case ro ~ _ ~ fn => TypeFromObject(ro, fn) }

  // ASN1D 15.6.2<13>
  def objectFromObject =
    ( referencedObjects ~ op(".") ~ fieldName
    ) ^^ { case ro ~ _ ~ fn => ObjectFromObject(ro, fn) }

  // ASN1D 15.6.2<15>
  def objectSetFromObjects =
    ( referencedObjects ~ op(".") ~ fieldName
    ) ^^ { case ro ~ _ ~ fn => ObjectSetFromObjects(ro, fn) }

  // ASN1D 15.6.2<19>
  def referencedObjects =
    ( definedObject
    | parameterizedObject
    | definedObjectSet
    | parameterizedObjectSet
    ) ^^ { kind => ReferencedObjects(kind) }
  // See ASN1D 15.2.2<33>
  
  // ASN1D 15.6.2<20>
  // See ASN1D 15.2.2<33>

  // ASN1D 15.7.2<1>
  def objectClassFieldType =
    ( definedObjectClass ~ op(".") ~ fieldName
    ) ^^ { case doc ~ _ ~ fn => ObjectClassFieldType(doc, fn) } // TODO
  // See ASN1D 15.2.2<33>
  
  // ASN1D 15.7.2<9>
  def objectClassFieldValue =
    ( openTypeFieldVal
    | fixedTypeFieldVal
    ) ^^ { kind => ObjectClassFieldValue(kind) }

  // ASN1D 15.7.2<11>
  def openTypeFieldVal =
    ( type_ ~ op(":") ~ value
    ) ^^ { case t ~ _ ~ v => OpenTypeFieldVal(t, v) }

  // ASN1D 15.7.2<13>
  def fixedTypeFieldVal =
    ( builtinValue
    | referencedValue
    ) ^^ { kind => FixedTypeFieldVal(kind) }

  // ASN1D 15.7.2<15>
  // See ASN1D 13.12.2<1>

  // ASN1D 15.7.2<16>
  // See ASN1D 13.12.2<5>
  def generalConstraint =
    ( userDefinedConstraint
    | tableConstraint
    | contentsConstraint
    ) ^^ { kind => GeneralConstraint(kind) }
  def tableConstraint =
    ( simpleTableConstraint
    | componentRelationConstraint
    ) ^^ { kind => TableConstraint(kind) }
  
  // ASN1D 15.7.2<17>
  def simpleTableConstraint =
    ( objectSet
    ) ^^ { os => SimpleTableConstraint(os) }

  // ASN1D 15.7.2<22>
  def componentRelationConstraint =	
    ( op("{")
    ~ definedObjectSet
    ~ op("}")
    ~ op("{")
    ~ rep1sep(atNotation, op(","))
    ~ op("}")
    ) ^^ { case _ ~ dos ~ _ ~ _ ~ ans ~ _ => ComponentRelationConstraint(dos, ans) }
  
  // ASN1D 15.7.2<24>
  def atNotation =
    ( op("@") ~ componentIdList ^^ { case _ ~ cil => AtNotation(cil, false) }
    | op("@.") ~ componentIdList ^^ { case _ ~ cil => AtNotation(cil, true) }
    )

  // ASN1D 15.7.2<28>
  def componentIdList =
    ( rep1sep(identifier, op("."))
    ) ^^ { identifiers => ComponentIdList(identifiers) }

  // ASN1D 15.7.2<36>
  def typeConstraint =
    ( type_
    ) ^^ { t => TypeConstraint(t) }

  // ASN1D 15.9<6>
  def instanceOfValue =
    ( value
    ) ^^ { v => InstanceOfValue(v) }
  
  // ASN1D 15.9.2<1>
  def usefulObjectClassReference =
    ( kw("TYPE-IDENTIFIER")
    | kw("ABSTRACT-SYNTAX")
    ) ^^ { _ => UsefulObjectClassReference() }
  
  // ASN1D 15.9.2<2>
  def instanceOfType =
    ( kw("INSTANCE") ~ kw("OF") ~ definedObjectClass
    ) ^^ { case _ ~ _ ~ doc => InstanceOfType(doc) } // TODO

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
    ) ^^ { kind => ParameterizedAssignment(kind) }
  
  // ASN1D 17.2.2<3>
  def parameterizedTypeAssignment =
    ( typeReference
    ~ parameterList
    ~ op("::=")
    ~ type_
    ) ^^ { case tr ~ pl ~ _ ~ t => ParameterizedTypeAssignment(tr, pl, t) }

  // ASN1D 17.2.2<5>
  def parameterizedValueAssignment =
    ( valueReference
    ~ parameterList
    ~ type_
    ~ op("::=")
    ~ value
    ) ^^ { case vr ~ pl ~ t ~ _ ~ v => ParameterizedValueAssignment(vr, pl, t, v) }

  // ASN1D 17.2.2<6>
  def parameterizedValueSetTypeAssignment =
    ( typeReference
    ~ parameterList
    ~ type_
    ~ op("::=")
    ~ valueSet
    ) ^^ { case tr ~ pl ~ t ~ _ ~ vs => ParameterizedValueSetTypeAssignment(tr, pl, t, vs) }

  // ASN1D 17.2.2<8>
  def parameterizedObjectClassAssignment =
    ( objectClassReference
    ~ parameterList
    ~ op("::=")
    ~ objectClass
    ) ^^ { case ocr ~ pl ~ _ ~ oc => ParameterizedObjectClassAssignment(ocr, pl, oc) }

  // ASN1D 17.2.2<9>
  def parameterizedObjectAssignment =
    ( objectReference
    ~ parameterList
    ~ definedObjectClass
    ~ op("::=")
    ~ object_
    ) ^^ { case or ~ pl ~ doc ~ _ ~ o => ParameterizedObjectAssignment(or, pl, doc, o) }

  // ASN1D 17.2.2<10>
  def parameterizedObjectSetAssignment =
    ( objectSetReference
    ~ parameterList
    ~ definedObjectClass
    ~ op("::=")
    ~ objectSet
    ) ^^ { case osr ~ pl ~ doc ~ _ ~ os => ParameterizedObjectSetAssignment(osr, pl, doc, os) }

  // ASN1D 17.2.2<11>
  def parameterList =
    ( op("{") ~ rep1sep(parameter, op(",")) ~ op("}")
    ) ^^ { case _ ~ parameters ~ _ => ParameterList(parameters) }
  def parameter =
    ( paramGovernor ~ op(":") ~ dummyReference
    | dummyReference
    ) ^^ { _ => Parameter() }
  
  // ASN1D 17.2.2<12>
  def paramGovernor =
    ( governor
    | dummyGovernor
    ) ^^ { kind => ParamGovernor(kind) }
  
  // Se ASN1D 13.13.2<7>
  def dummyGovernor =
    ( dummyReference
    ) ^^ { dg => DummyGovernor(dg) }
  def dummyReference =
    ( reference
    ) ^^ { r => DummyReference(r) }
  
  // ASN1D 17.2.2<25>
  def parameterizedType =
    ( simpleDefinedType ~ actualParameterList
    ) ^^ { case sdt ~ apl => ParameterizedType(sdt, apl) }
  def parameterizedValueSetType =
    ( simpleDefinedType ~ actualParameterList
    ) ^^ { case sdt ~ apl => ParameterizedValueSetType(sdt, apl) }
  def simpleDefinedType =
    ( externalTypeReference
    | typeReference
    ) ^^ { kind => SimpleDefinedType(kind) }
  
  // ASN1D 17.2.2<27>
  def parameterizedValue =
    ( simpleDefinedValue ~ actualParameterList
    ) ^^ { case sdv ~ apl => ParameterizedValue(sdv, apl) }
  
  def simpleDefinedValue =
    ( externalValueReference
    | valueReference
    ) ^^ { kind => SimpleDefinedValue(kind) }
  
  // ASN1D 17.2.2<29>
  def parameterizedObjectClass =
    ( definedObjectClass ~ actualParameterList
    ) ^^ { case doc ~ apl => ParameterizedObjectClass(doc, apl) }

  // ASN1D 17.2.2<30>
  def parameterizedObject =
    ( definedObject ~ actualParameterList
    ) ^^ { case definedObject ~ apl => ParameterizedObject(definedObject, apl) }

  // ASN1D 17.2.2<31>
  def parameterizedObjectSet =
    ( definedObjectSet ~ actualParameterList
    ) ^^ { case dos ~ apl => ParameterizedObjectSet(dos, apl) }

  // ASN1D 17.2.2<32>
  def actualParameterList =
    ( op("{") ~ rep1sep(actualParameter, op(",")) ~ op("}")
    ) ^^ { case _ ~ parameters ~ _ => ActualParameterList(parameters) }
  
  def actualParameter =
    ( type_
    | value
    | valueSet
    | definedObjectClass
    | object_
    | objectSet
    ) ^^ { kind => ActualParameter(kind) }
}
