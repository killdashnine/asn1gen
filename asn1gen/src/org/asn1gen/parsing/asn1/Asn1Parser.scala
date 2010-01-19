package org.asn1gen.parsing.asn1

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

import org.asn1gen.parsing.asn1.ast._
import org.asn1gen.parsing.syntax._

class Asn1Parser extends Asn1ParserBase with ImplicitConversions {
  // ASN1D: 8.2.3<15-16>
  // TODO: not implemented
  
  // ASN1D: 8.2.3<17>
  def moduleReference =
    ( typeReference
    ) ^^ { case tr@TypeReference(_) => tr.asModuleReference }
  
  // ASN1D: 8.2.3<19>
  def objectClassReference =
    ( typeReference
    ) ^^ { case TypeReference(n) => ObjectClassReference(n) }
  
  // ASN1D: 8.2.3<20>
  // TODO: unsure if specification means there should be no space after '&'
  def objectFieldReference =
    ( op("&") ~> valueFieldReference
    ) ^^ { vfr => ObjectFieldReference(vfr) }
  
  // ASN1D: 8.2.3<21>
  def objectReference =
    ( valueReference
    ) ^^ { case ValueReference(n) => ObjectReference(n) }

  // ASN1D: 8.2.3<22>
  def objectSetFieldReference =
    ( op("&") ~> objectSetReference
    ) ^^ { case ObjectSetReference(n) => ObjectSetFieldReference(n) }

  // ASN1D: 8.2.3<23>
  def objectSetReference =
    ( typeReference
    ) ^^ { case TypeReference(n) => ObjectSetReference(n) }
  
  // ASN1D: 8.2.3<24>
  
  def signedNumber =
    ( ( op("-") ~ number
      ) ^? { case _ ~ number if number.chars != "0" =>
        SignedNumber(true, number)
      }
    | number ^^ { number =>
        SignedNumber(false, number)
      }
    )
  
  // ASN1D: 8.2.3<25>
  def typeFieldReference =
    ( op("&") ~> typeReference
    ) ^^ { case TypeReference(n) => TypeFieldReference(n) }
  
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
    ( op("&") ~> valueReference
    ) ^^ {
      case ValueReference(n) => ValueFieldReference(n)
    }

  // ASN1D: 8.2.3<33>
  def valueSetFieldReference =
    ( op("&") ~> typeReference
    ) ^^ {
      case TypeReference(n) => ValueSetFieldReference(n)
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
    )
  
  // ASN1D: 9.1.2<3>
  
  def typeAssignment =
    ( typeReference
    ~ op("::=")
    ~ _type
    ) ^^ { case n ~ _ ~ t => TypeAssignment(n, t) } | failure("type assignment")
  
  def _type : Parser[Type] =
    ( ( builtinType
      | referencedType
      | constrainedType
      )
    ~ constraint.* // refactored from constrainedType
    ) ^^ { case kind ~ constraints => Type(kind, constraints) }
  
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
    )
  
  def referencedType: Parser[ReferencedType] =
    ( definedType
    | usefulType
    | selectionType
    | typeFromObject
    | valueSetFromObjects
    )
  
  // ASN1D 9.1.2<4>
  def valueAssignment =
    ( valueReference
    ~ _type
    ~ op("::=")
    ~ value
    ) ^^ { case vr ~ t ~ _ ~ v => ValueAssignment(vr, t, v) }
  
  // ASN1D 9.1.2<5>
  def value: Parser[Value] =
    ( builtinValue ||| referencedValue
    )
  
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
    )
  
  def referencedValue: Parser[ReferencedValue] =
    ( valueFromObject // refactored
    | definedValue // refactored
    )

  def taggedValue =
    ( failure("force fail to prevent recursion") ~ value
    ) ^^ { case _ ~ v => TaggedValue(v) }
  
  def valueSetTypeAssignment =
    ( typeReference
    ~ _type
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
    ~ kwDefinitions
    ~ tagDefault
    ~ extensionDefault
    ~ op("::=")
    ~ kwBegin
    ~ moduleBody
    ~ kwEnd
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
  def definitiveObjectIdComponent: Parser[DefinitiveObjectIdComponent] =
    ( definitiveNameAndNumberForm
    | definitiveNumberForm
    | nameForm
    )
  
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
    ( ( kwExtensibility
      ~ kwImplied
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
    ( kwExports
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
    ( kwImports ~> symbolsImported <~ op(";")
    ).? ^^ { case si => Imports(si) }

  def symbolsImported =
    ( symbolsFromModule.*
    ) ^^ { sfms => SymbolsImported(sfms) }
  
  def symbolsFromModule =
    ( rep1sep(symbol, op(","))
    ~ kwFrom
    ~ globalModuleReference
    ) ^^ { case ss ~ _ ~ gmr => SymbolsFromModule(ss, gmr) }
  
  // ASN1D 9.2.2<22>
  def globalModuleReference =
    ( moduleReference
    ~ assignedIdentifier
    ) ^^ { case mr ~ ai => GlobalModuleReference(mr, ai) }
  
  // ASN1D 9.2.2<27>
  def assignedIdentifier: Parser[AssignedIdentifier] =
    ( objectIdentifierValue
    | definedValue
    | empty
    )

  // ASN1D 9.2.2<30> // refactored
  def symbol: Parser[Symbol] =
    ( parameterizedReference
    | reference
    )

  def reference: Parser[Reference] =
    ( typeReference
    | valueReference
    | objectClassReference
    | objectReference
    | objectSetReference
    )
  
  def parameterizedReference =
    ( reference
    ~ (op("{") ~ op("}")).?
    ) ^^ { case r ~ b => ParameterizedReference(r, b.isDefined) }
  
  // ASN1D 9.3.2<1> refactored
  def definedType: Parser[DefinedType] =
    ( parameterizedType
    | parameterizedValueSetType
    | externalTypeReference
    | typeReference
    )

  // ASN1D 9.3.2<3>
  def externalTypeReference =
    ( moduleReference
    ~ op(".")
    ~ typeReference
    ) ^^ { case mr ~ _ ~ tr => ExternalTypeReference(mr, tr) }
  
  // ASN1D 9.3.2<8>
  def definedValue: Parser[DefinedValue] =
    ( externalValueReference
    | valueReference
    | parameterizedValue
    )
  
  // ASN1D 9.3.2<10>
  def externalValueReference =
    ( moduleReference
    ~ op(".")
    ~ valueReference
    ) ^^ { case mr ~ _ ~ vr => ExternalValueReference(mr, vr) }
  
  // ASN1D 9.3.2<14>
  def definedObjectClass: Parser[DefinedObjectClass] =
    ( externalObjectClassReference
    | objectClassReference
    | usefulObjectClassReference
    )
  
  // ASN1D 9.3.2<15>
  def externalObjectClassReference =
    ( moduleReference
    ~ op(".")
    ~ objectClassReference
    ) ^^ { case mr ~ _ ~ ocr => ExternalObjectClassReference(mr, ocr) }
  
  // ASN1D 9.3.2<19>
  def definedObject: Parser[DefinedObject] =
    ( externalObjectReference
    | objectReference
    )
  
  // ASN1D 9.3.2<20>
  def externalObjectReference =
    ( moduleReference
    ~ op(".")
    ~ objectReference
    ) ^^ { case mr ~ _ ~ or => ExternalObjectReference(mr, or) }
  
  // ASN1D 9.3.2<24>
  def definedObjectSet: Parser[DefinedObjectSet] =
    ( externalObjectSetReference
    | objectSetReference
    )
  
  def externalObjectSetReference =
    ( moduleReference
    ~ op(".")
    ~ objectSetReference
    ) ^^ { case mr ~ _ ~ osr => ExternalObjectSetReference(mr, osr) }
  
  // ASN1D 10.1.2
  def booleanType =
    ( kwBoolean
    )
  
  def booleanValue = kwTrue | kwFalse
  
  // ASN1D 10.2.2
  def nullType =
    ( kwNull
    )
  
  def nullValue =
    ( kwNull
    )
  
  // ASN1D 10.3.2<1>
  def integerType =
    ( kwInteger
    ~ ( op("{") ~> rep1sep(namedNumber, op(",")) <~ op("}")
      ).?
    ) ^^ { case _ ~ nns => INTEGER(nns) }
  
  // ASN1D 10.3.2<6>
  def namedNumber =
    ( identifier
    ~ op("(")
    ~ (signedNumber | definedValue)
    ~ op(")")
    ) ^^ { case id ~ _ ~ value ~ _ => NamedNumber(id, value) }
  
  // ASN1D 10.3.2<11>
  def integerValue: Parser[IntegerValue] =
    ( signedNumber
    | identifier
    )
  
  // ASN1D 10.4.2<1>
  def enumeratedType =
    ( kwEnumerated
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
  
  // refactored
  def enumerationItem: Parser[EnumerationItem] =
    ( namedNumber
    | identifier
    )
  
  // ASN1D 10.4.2<13>
  // See 10.3.2<6>
  // Not implemented
  
  // ASN1D 10.4.2<16>
  def enumeratedValue =
    ( identifier
    ) ^^ { i => EnumeratedValue(i) }
  
  // ASN1D 10.5.2<1>
  def realType = kwReal
  
  // ASN1D 10.5.2<5>
  def realValue: Parser[RealValue] =
    ( numericRealValue
    | specialRealValue
    )
  
  def numericRealValue: Parser[NumericRealValue] =
    ( number ^? { case Number("0") => Number("0") }
    | sequenceValue
    )
  
  // ASN1D 10.5.2<8>
  def specialRealValue = kwPlusInfinity | kwMinusInfinity
  
  // ASN1D 10.6.2<1>
  def bitStringType =
    ( kw("BIT")
    ~ kw("STRING")
    ~ ( op("{") ~> rep1sep(namedBit, op(",")) <~ op("}")
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
  def bitStringValue: Parser[BitStringValue] =
    ( bstring
    | hstring
    | identifierList
    )
  
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
    ) ^^ { _ => OctetStringType }
  
  // ASN1D 10.7.2<4>
  def octetStringValue: Parser[OctetStringValue] =
    ( bstring
    | hstring
    )
  
  // ASN1D 10.8.2<1>
  def objectIdentifierType =
    ( kw("OBJECT")
    ~ kw("IDENTIFIER")
    ) ^^ { _ => ObjectIdentifierType }
  
  // ASN1D 10.8.2<3> refactored
  def objectIdentifierValueData =
    ( ( objIdComponents.+
      ) ^^ { oic => ObjectIdentifierValue(None, oic) }
    | ( definedValue
      ~ objIdComponents.+
      ) ^^ { case dv ~ oic => ObjectIdentifierValue(Some(dv), oic) }
    )
  def objectIdentifierValue =
    ( op("{") ~> objectIdentifierValueData <~ op("}")
    )
  
  // ASN1D 10.8.2<5> refactored
  def objIdComponents: Parser[ObjIdComponents] =
    ( nameAndNumberForm
    | nameForm
    | numberForm
    | definedValue
    )
  
  // ASN1D 10.8.2<6>
  // See ASN1D 9.2.2<6>
  
  // ASN1D 10.8.6<8>
  def numberForm: Parser[NumberForm] =
    ( number
    | definedValue
    )
  
  // ASN1D 10.8.6<10>
  def nameAndNumberForm =
    ( identifier
    ~ op("(")
    ~ numberForm
    ~ op(")")
    ) ^^ { case i ~ _ ~ n ~ _ => NameAndNumberForm(i, n) }
  
  // ASN1D 10.9.2<2>
  def relativeOidType = kwRelativeOid
  
  // ASN1D 10.9.2<4>
  def relativeOidValue =
    ( op("{") ~> relativeOidComponents.+ <~ op("}")
    ) ^^ { rocs => RelativeOidValue(rocs) }
  
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
  def restrictedCharacterStringValue: Parser[RestrictedCharacterStringValue] =
    ( cstring
    | characterStringList
    | quadruple
    | tuple
    )
  
  def characterStringList =
    ( op("{") ~> rep1sep(charsDefn, op(",")) <~ op("}")
    ) ^^ { cds => CharacterStringList(cds) }
  
  def charsDefn: Parser[CharsDefn] =
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
  def characterStringType: Parser[CharacterStringType] =
    ( restrictedCharacterStringType
    | unrestrictedCharacterStringType
    )

  def restrictedCharacterStringType: Parser[RestrictedCharacterStringType] =
    ( kwBMPString
    | kwGeneralString
    | kwIA5String
    | kwNumericString
    | kwTeletexString
    | kwUniversalString
    | kwVideotexString
    | kwGraphicString
    | kwISO646String
    | kwPrintableString
    | kwT61String
    | kwUTF8String
    | kwVisibleString
    )

  // ASN1D 11.13<36>
  def characterStringValue: Parser[CharacterStringValue] =
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
    ) ^^ { case _ ~ tc ~ _ ~ tr ~ _ => Tuple(tc, tr) }
  
  def tableColumn =
    ( number
    ) ^^ { n => TableColumn(n) }
  
  def tableRow =
    ( number
    ) ^^ { n => TableRow(n) }
  
  // ASN1D 11.15.2
  def usefulType = kwGeneralizedTime | kwUTCTime | kwObjectDescriptor
  
  // ASN1D 11.16.2<1>
  // See ASN1D 11.15.2<1>
  
  // ASN1D 11.17.2<1>
  // See ASN1D 11.15.2<1>

  // ASN1D 12.1.4<1>
  def taggedType =
    ( tag
    ~ (kwImplicit | kwExplicit | empty)
    ~ _type
    ) ^^ { case tag ~ taggedKind ~ t => TaggedType(tag, taggedKind, t) }
  
  def tag =
    ( op("[")
    ~ class_
    ~ classNumber
    ~ op("]")
    ) ^^ { case _ ~ c ~ cn ~ _ => Tag(c, cn) }
  
  def classNumber: Parser[ClassNumber] =
    ( number
    | definedValue
    )
  
  // ASN1D 12.1.4<7>
  def class_ =
    ( kwUniversal
    | kwApplication
    | kwPrivate
    | empty
    )
  
  // ASN1D 12.1.4<15>
  // See ASN1D 9.2.2<1>
  def tagDefault: Parser[TagDefault] =
    ( ( kwExplicit
      | kwImplicit
      | kwAutomatic
      ) <~ kw("TAGS")
    | empty
    )

  // ASN1D 12.2.2<1>
  def extensionSequenceTypeSpec =
    ( extensionAndException
    ~ optionalExtensionMarker
    ) ^^ { case ea ~ oem => ExtensionSequenceTypeSpec(ea, oem) }
  
  def sequenceTypeSpec =
    ( extensionSequenceTypeSpec
    | componentTypeLists
    | empty
    )
	
  def sequenceType =
    ( kwSequence ~> op("{") ~> sequenceTypeSpec <~ op("}")
    ) ^^ { spec => SequenceType(spec) }
  
  // ASN1D 12.2.2<4> refactored new
  def componentTypeListsExtension =
    ( extensionAndException
    ~ extensionsAdditions
    ~ optionalExtensionMarker
    ) ^^ { case ee ~ ea ~ oem => ComponentTypeListsExtension(ee, ea, oem) }

  // refactored
  def componentTypeLists =
    ( ( rootComponentTypeList
      ~ ( op(",")
        ~>componentTypeListsExtension
        ~ ( op(",")
          ~>rootComponentTypeList
          ).?
        ).?
      ) ^^ {
        case l1 ~ None => ComponentTypeLists(Some(l1), None, None)
        case l1 ~ Some(e ~ l2) => ComponentTypeLists(Some(l1), Some(e), l2)
      }
    | ( componentTypeListsExtension
      ~ op(",")
      ~ rootComponentTypeList
      ) ^^ { case e ~ _ ~ l => ComponentTypeLists(None, Some(e), Some(l)) }
    )
  
  // ASN1D 12.2.2<5>
  def rootComponentTypeList: Parser[ComponentTypeList] =
    ( componentTypeList
    )
  
  def extensionsAdditions =
    ( op(",") ~> extensionAdditionList
    ).? ^^ { eal => ExtensionsAdditions(eal) }

  def extensionAdditionList =
    ( rep1sep(extensionAddition, op(","))
    ) ^^ { eas => ExtensionAdditionList(eas) }
  def extensionAddition: Parser[ExtensionAddition] =
    ( componentType
    | extensionAdditionGroup
    )
  def extensionAdditionGroup =
    ( op("[[") ~> componentTypeList <~ op("]", "]")
    ) ^^ { ctl => ExtensionAdditionGroup(ctl) }
  def componentTypeList =
    ( rep1sep(componentType, op(","))
    ) ^^ { cts => ComponentTypeList(cts) }
  
  // ASN1D 12.2.2<13> refactored
  def namedComponentType =
    ( namedType
    ~ ( kwOptional 
      | default(value)
      | empty
      )
    ) ^^ { case nt ~ od => NamedComponentType(nt, od) }
    
  def basicComponentType =
    ( kw("COMPONENTS") ~> kw("OF") ~> _type
    ) ^^ { t => BasicComponentType(t) }
    
  def componentType =
    ( namedComponentType
    | basicComponentType
    )
  
  // ASN1D 12.2.2<24>
  def namedType =
    ( identifier ~ _type
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
  def setTypeExtension =
    ( extensionAndException ~ optionalExtensionMarker
    ) ^^ { case eae ~ oem => SetTypeExtension(eae, oem) }
  
  def setTypeSpec: Parser[SetTypeSpec] =
    ( setTypeExtension
    | componentTypeLists
    )

  def setType =
    ( kwSet ~> op("{") ~> setTypeSpec <~ op("}")
    ) ^^ { spec => SetType(spec) }

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
    ( kwSequence ~ kw("OF") ~ _type
    ) ^^ { case _ ~ _ ~ t => SequenceOfType(t) } // TODO

  // ASN1D 12.4.2<3>
  def typeWithConstraint =
    ( ( kwSequence
      | kwSet)
    ~ ( constraint
      | sizeConstraint
      )
    ~ kw("OF")
    ~ _type
    ) ^^ { case ct ~ c ~ _ ~ t => TypeWithConstraint(ct, c, t) }
  
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
    ( kwSet ~ kw("OF") ~ _type
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
    ( rootAlternativeTypeList
    ~ ( op(",")
      ~ extensionAndException
      ~ extensionAdditionAlternatives
      ~ optionalExtensionMarker
      ).?
    ) ^^ {
      case ratl ~ Some(_ ~ eae ~ eaa ~ oem) =>
        AlternativeTypeLists(ratl, Some(eae), Some(eaa), Some(oem))
      case ratl ~ None =>
        AlternativeTypeLists(ratl, None, None, None)
    }
  
  // ASN1D 12.6.2<4>
  def rootAlternativeTypeList =
    ( alternativeTypeList
    ) ^^ { atl => RootAlternativeTypeList(atl) }
  def extensionAdditionAlternatives =
    ( op(",") ~> extensionAdditionAlternativesList
    | empty
    )

  def extensionAdditionAlternativesList =
    ( rep1sep(extensionAdditionAlternative, op(","))
    ) ^^ { eaas => ExtensionAdditionAlternativesList(eaas) }

  def extensionAdditionAlternative: Parser[ExtensionAdditionAlternative] =
    ( extensionAdditionGroupAlternatives
    | namedType
    )

  def extensionAdditionGroupAlternatives =
    ( op("[[")
    ~ alternativeTypeList
    ~ op("]", "]")
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
    ( identifier ~ op("<") ~ _type
    ) ^^ { case i ~ _ ~ t => SelectionType(i, t) }

  // ASN1D 12.9.2<1>
  def extensionAndException =
    ( op("...") ~> exceptionSpec.?
    ) ^^ { es => ExtensionAndException(es) }

  def optionalExtensionMarker =
    ( op(",") ~ op("...") ^^ { _ => true }
    | empty ^^ { _ => false }
    ) ^^ { exists => OptionalExtensionMarker(exists) } // TODO
  
  // ASN1D 12.9.2<18>
  def exceptionSpec: Parser[ExceptionSpec] =
    ( op("!") ~> exceptionIdentification
    | empty
    )

  // ASN1D 12.9.2<20>
  def exceptionIdentificationTypeAndValue =
    ( _type ~ op(":") ~ value
    ) ^^ { case t ~ _ ~ v => ExceptionIdentificationTypeAndValue(t, v) }

  def exceptionIdentification =
    ( signedNumber
    | definedValue
    | exceptionIdentificationTypeAndValue
    )
  
  // ASN1D 13.1.2<1>
  def constrainedType =
    ( /*failure("refactor to _type") ~ _type ~ constraint
    |*/ typeWithConstraint
    ) ^^ { twc => ConstrainedType(twc) } // TODO

  // ASN1D 13.2.2<1>
  def singleValue =
    ( value
    ) ^^ { v => SingleValue(v) }

  // ASN1D 13.3.2<1>
  def containedSubtype =
    ( includes ~ _type
    ) ^^ { case i ~ t => ContainedSubtype(i.isDefined, t) }

  // ASN1D 13.3.7<7>
  def includes =
    ( kwIncludes.?
    )

  // ASN1D 13.4.2<1>
  def valueRange =
    ( lowerEndPoint ~ op("..") ~ upperEndPoint
    ) ^^ { case lep ~ _ ~ uep => ValueRange(lep, uep) }

  // ASN1D 13.4.2<3> refactored
  def lowerEndPoint =
    ( lowerEndValue ~ op("<").?
    ) ^^ { case lev ~ exclusive => LowerEndPoint(exclusive.isDefined, lev) }
  // refactored
  def upperEndPoint =
    ( op("<").? ~ upperEndValue
    ) ^^ { case exclusive ~ uev => UpperEndPoint(exclusive.isDefined, uev) }
  
  // ASN1D 13.4.2<4>
  def lowerEndValue: Parser[LowerEndValue] =
    ( value | kwMin
    )
  def upperEndValue: Parser[UpperEndValue] =
    ( value | kwMax
    )
  
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
    ( kw("WITH")
    ~>( kw("COMPONENT") ~> singleTypeConstraint
      | kw("COMPONENTS") ~> multipleTypeConstraints
      )
    )
  
  // ASN1D 13.8.2<3>
  def singleTypeConstraint =
    ( constraint
    ) ^^ { c => SingleTypeConstraint(c) }

  // ASN1D 13.9.2<1>
  // See ASN1D 13.8.2<1>
  
  // ASN1D 13.9.2<4>
  def multipleTypeConstraints: Parser[MultipleTypeConstraints] =
    ( fullSpecification
    | partialSpecification
    )

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

  // ASN1D 13.9.2<10> refactored
  def namedConstraint =
    ( identifier
    ~ componentConstraint
    ) ^^ { case i ~ cc => NamedConstraint(i, cc) }

  // ASN1D 13.9.2<12>
  def componentConstraint =
    ( valueConstraint
    ~ presenceConstraint
    ) ^^ { case vc ~ pc => ComponentConstraint(vc, pc) }
  def valueConstraint: Parser[ValueConstraint] =
    ( constraint
    | empty
    )
  
  // ASN1D 13.9.2<14>
  def presenceConstraint =
    ( kwPresent
    | kwAbsent
    | kwOptional
    | empty
    )
  
  // ASN1D 13.10.2<1>
  def contentsConstraint =
    ( ( kw("CONTAINING")
      ~>_type
      ~ ( kw("ENCODED")
        ~>kw("BY")
        ~>value
        ).?
      ) ^^ { case t ~ v => ContentsConstraint(Some(t), v) }
    | ( kw("ENCODED")
      ~>kw("BY")
      ~>value
      ) ^^ { v => ContentsConstraint(None, Some(v)) }
    )
  
  // ASN1D 13.11.2<1> refactored
  def elementSetSpecs: Parser[ElementSetSpecs] =
    ( rootElementSetSpec
    ~ ( op(",")
      ~>op("...")
      ~>( op(",")
        ~>additionalElementSetSpec
        ).?
      ).?
    ) ^^ { case ress ~ aess => ElementSetSpecs(ress, aess) }
  
  // ASN1D 13.11.2<2>
  def rootElementSetSpec =
    ( elementSetSpec
    ) ^^ { ess => RootElementSetSpec(ess) }
  def additionalElementSetSpec =
    ( elementSetSpec
    ) ^^ { ess => AdditionalElementSetSpec(ess) }
  
  // ASN1D 13.11.2<9> refactored
  def elementSetSpec: Parser[ElementSetSpec] =
    ( kw("ALL") ~> exclusions
    | unions
    )

  // ASN1D 13.11.2<10> refactored
  def unions: Parser[Unions] =
    ( rep1sep(intersections, unionMark)
    ) ^^ { i => Unions(i) }

  // ASN1D 13.11.2<11>
  // uElems refactored out
  def unionMark =
    ( op("|")
    | kw("UNION")
    ) ^^ { _ => UnionMark }
 
  // ASN1D 13.11.2<12> refactored
  def intersections: Parser[Intersections] =
    ( rep1sep(intersectionElements, intersectionMark)
    ) ^^ { ie => Intersections(ie) }
  
  // ASN1D 13.11.2<13>
  // iElems refactored out
  def intersectionMark =
    ( op("^")
    | kw("INTERSECTION")
    ) ^^ { _ => IntersectionMark }
  def intersectionElements =
    ( elements
    ~ exclusions.?
    ) ^^ { case el ~ ex => IntersectionElements(el, ex) }
  
  // ASN1D 13.11.2<14>

  def exclusions =
    ( kw("EXCEPT") ~ elements
    ) ^^ { case _ ~ e => Exclusions(e) }

  // ASN1D 13.11.2<16>
  def elements =
    ( subtypeElements
    | objectSetElements
    | op("(") ~> elementSetSpec <~ op(")")
    )
  
  // ASN1D 13.11.2<17>
  def subtypeElements =
    ( valueRange // Must come before (singleValue)
    | singleValue
    | containedSubtype
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
  def constraintSpec: Parser[ConstraintSpec] =
    ( generalConstraint
    | elementSetSpecs
    )

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
  def governorConstraintParameterValue =
    ( value
    | valueSet
    | object_
    | objectSet
    )

  def governorConstraintParameter =
    ( governor ~ op(":")
    ~ governorConstraintParameterValue
    ) ^^ { case g ~ _ ~ gcpv => GovernorConstraintParameter(g, gcpv) }

  def userDefinedConstraintParameter	=
    ( governorConstraintParameter
    | _type
    | definedObjectClass
    )
  
  // ASN1D 13.13.2<7>
  def governor =
    ( _type
    | definedObjectClass
    ) ^^ { kind => Governor(kind) }

  // ASN1D 14.1.2<1>
  def externalType = kwExternal

  // ASN1D 14.1.2<7>
  def externalValue =
    ( sequenceValue
    ) ^^ { sv => ExternalValue(sv) }

  // ASN1D 14.2.2<1>
  def embeddedPdvType =
    ( kw("EMBEDDED") ~ kw("PDV")
    ) ^^ { _ => EmbeddedPdvType }

  // ASN1D 14.2.2<5>
  def embeddedPdvValue =
    ( sequenceValue
    ) ^^ { sv => EmbeddedPdvValue(sv) }

  // ASN1D 14.3.2<1>
  def unrestrictedCharacterStringType =
    ( kw("CHARACTER") ~ kw("STRING")
    ) ^^ { _ => UnrestrictedCharacterStringType }

  // ASN1D 14.3.2<6>
  def unrestrictedCharacterStringValue =
    ( sequenceValue
    ) ^^ { sv => UnrestrictedCharacterStringValue(sv) }

  // ASN1D 15.2.2<1>
  def objectClass: Parser[ObjectClass] =
    ( definedObjectClass
    | objectClassDefn
    | parameterizedObjectClass
    )
  def objectClassDefn =
    ( kw("CLASS")
    ~ op("{")
    ~ rep1sep(fieldSpec, op(","))
    ~ op("}")
    ~ withSyntaxSpec
    ) ^^ { case _ ~ _ ~ fieldSpecs ~ _ ~ wss => ObjectClassDefn(fieldSpecs, wss) }

  def fieldSpec =
    ( fixedTypeValueFieldSpec
    | variableTypeValueFieldSpec
    | fixedTypeValueSetFieldSpec
    | variableTypeValueSetFieldSpec
    | typeFieldSpec // refactored
    | objectFieldSpec
    | objectSetFieldSpec
    )
  
  // ASN1D 15.2.2<3>
  def typeFieldSpec =
    ( typeFieldReference ~ typeOptionalitySpec
    ) ^^ { case tfr ~ tos => TypeFieldSpec(tfr, tos) }
  def typeOptionalitySpec =
    ( kwOptional
    | default(_type)
    | empty
    ) ^^ { value => TypeOptionalitySpec(value) }
  
  // ASN1D 15.2.2<6>
  def fixedTypeValueFieldSpec =
    ( valueFieldReference
    ~ _type
    ~ unique
    ~ valueOptionalitySpec
    ) ^^ { case vfr ~ t ~ u ~ vos => FixedTypeValueFieldSpec(vfr, t, u, vos) }

  // ASN1D 15.2.2<7>
  def unique = kwUnique.?

  // ASN1D 15.2.2<10>
  def valueOptionalitySpec =
    ( kwOptional
    | default(value)
    | empty
    ) ^^ { value => ValueOptionalitySpec(value) }
  
  // ASN1D 15.2.2<13>
  def variableTypeValueFieldSpec =
    ( valueFieldReference ~ fieldName ~ valueOptionalitySpec
    ) ^^ { case vfr ~ fn ~ vos => VariableTypeValueFieldSpec(vfr, fn, vos) }

  // ASN1D 15.2.2<17>
  def fixedTypeValueSetFieldSpec =
    ( valueSetFieldReference ~ _type ~ valueSetOptionalitySpec
    ) ^^ { case vsfr ~ t ~ vsos => FixedTypeValueSetFieldSpec(vsfr, t, vsos) }

  // ASN1D 15.2.2<18>
  def valueSetOptionalitySpec =
    ( kwOptional
    | default(valueSet)
    | empty
    ) ^^ { value => ValueSetOptionalitySpec(value) }
  
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
    ( kwOptional
    | default(object_)
    | empty
    ) ^^ { oos => ObjectOptionalitySpec(oos) }
  
  // ASN1D 15.2.2<28>
  def objectSetFieldSpec =
    ( objectSetFieldReference ~ definedObjectClass ~ objectSetOptionalitySpec
    ) ^^ { case osfr ~ doc ~ osos => ObjectSetFieldSpec(osfr, doc, osos) }
  
  // ASN1D 15.2.2<29>
  def objectSetOptionalitySpec : Parser[ObjectSetOptionalitySpec] =
    ( kwOptional
    | default(objectSet)
    | empty
    ) ^^ { osos => ObjectSetOptionalitySpec(osos) }
  
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
  def objectDefn: Parser[ObjectDefn] =
    ( defaultSyntax
    | definedSyntax
    )

  // ASN1D 15.2.2<36>
  def defaultSyntax =
    ( op("{") ~ repsep(fieldSetting, op(",")) ~ op("}")
    ) ^^ { case _ ~ fss ~ _ => DefaultSyntax(fss) }
  def fieldSetting =
    ( primitiveFieldName ~ setting
    ) ^^ { case pfn ~ s => FieldSetting(pfn, s) }

  // ASN1D 15.2.2<38>
  def setting: Parser[Setting] =
    ( _type
    | value
    | valueSet
    | object_
    | objectSet
    )
  
  // ASN1D 15.3.2<1>
  def withSyntaxSpec: Parser[WithSyntaxSpec] =
    ( kw("WITH") ~> kw("SYNTAX") ~> syntaxList
    | empty
    )

  // ASN1D 15.3.2<2>
  def syntaxList =
    ( op("{") ~ tokenOrGroupSpec.+ ~ op("}")
    ) ^^ { case _ ~ togss ~ _ => SyntaxList(togss) }
  def tokenOrGroupSpec: Parser[TokenOrGroupSpec] =
    ( requiredToken
    | optionalGroup
    )
  def requiredToken: Parser[RequiredToken] =
    ( literal
    | primitiveFieldName
    )
  
  // ASN1D 15.3.2<3>
  def optionalGroup: Parser[OptionalGroup] =
    ( op("[") ~ tokenOrGroupSpec.+ ~ op("]")
    ) ^^ { case _ ~ togss ~ _ => OptionalGroup(togss) }

  // ASN1D 15.3.2<8>
  def literal =
    ( word ^^ { w => Some(w) }
    | op(",") ^^ { _ => None }
    ) ^^ { w => Literal(w) }

  // ASN1D 15.3.2<11>
  def definedSyntax =
    ( op("{") ~ definedSyntaxToken.* ~ op("}")
    ) ^^ { case _ ~ dsts ~ _ => DefinedSyntax(dsts) }
  def definedSyntaxToken: Parser[DefinedSyntaxToken] =
    ( setting
    | literal
    )
  // See ASN1D 15.3.2<8>
  // See ASN1D 15.2.2<38>
  
  // ASN1D 15.5.2<1>
  def objectSet: Parser[ObjectSet] =
    ( op("{") ~ objectSetSpec ~ op("}")
    ) ^^ { case _ ~ oss ~ _ => ObjectSet(oss) }

  // ASN1D 15.5.2<2>
  def objectSetSpec =
    ( ( rootElementSetSpec
      ~ ( op(",")
        ~>op("...")
        ~>( op(",")
          ~>additionalElementSetSpec
          ).?
        ).? 
      ) ^^ {
        case re ~ Some(aess) => (Some(re), aess)
        case re ~ None => (Some(re), None)
      }
    | ( op("...")
      ~>( op(",")
        ~>additionalElementSetSpec
        ).?
      ) ^^ { case aess => (None, aess) }
    ) ^^ { case (re, aess) => ObjectSetSpec(re, aess) }
  
  // ASN1D 15.5.2<11>
  def valueSet =
    ( op("{") ~> elementSetSpecs <~ op("}")
    ) ^^ { ess => ValueSet(ess) }
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
  def referencedObjects: Parser[ReferencedObjects] =
    ( definedObject
    | parameterizedObject
    | definedObjectSet
    | parameterizedObjectSet
    )
  // See ASN1D 15.2.2<33>
  
  // ASN1D 15.6.2<20>
  // See ASN1D 15.2.2<33>

  // ASN1D 15.7.2<1>
  def objectClassFieldType =
    ( definedObjectClass ~ op(".") ~ fieldName
    ) ^^ { case doc ~ _ ~ fn => ObjectClassFieldType(doc, fn) }
  // See ASN1D 15.2.2<33>
  
  // ASN1D 15.7.2<9>
  def objectClassFieldValue =
    ( openTypeFieldVal
    | failure("force fail to prevent infinite recursion") ~> fixedTypeFieldVal
    ) ^^ { kind => ObjectClassFieldValue(kind) }

  // ASN1D 15.7.2<11>
  def openTypeFieldVal =
    ( _type ~ op(":") ~ value
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
  def generalConstraint: Parser[GeneralConstraint] =
    ( userDefinedConstraint
    | tableConstraint
    | contentsConstraint
    )
  def tableConstraint: Parser[TableConstraint] =
    ( componentRelationConstraint
    | simpleTableConstraint
    )
  
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
    ( _type
    ) ^^ { t => TypeConstraint(t) }

  // ASN1D 15.9<6> refactored
  def instanceOfValue =
    ( failure("refactored to fail to prevent recursion")
    ~ value
    ) ^^ { case _ ~ v => InstanceOfValue(v) }
  
  // ASN1D 15.9.2<1>
  def usefulObjectClassReference = kwTypeIdentifier | kwAbstractSyntax
  
  // ASN1D 15.9.2<2>
  def instanceOfType =
    ( kw("INSTANCE") ~ kw("OF") ~ definedObjectClass
    ) ^^ { case _ ~ _ ~ doc => InstanceOfType(doc) }

  // ASN1D 15.10.2<1>
  // See ASN1D 15.9.2<1>

  // ASN1D 17.2.2<1>
  def parameterizedAssignment: Parser[ParameterizedAssignment] =
    ( parameterizedTypeAssignment
    | parameterizedValueAssignment
    | parameterizedValueSetTypeAssignment
    | parameterizedObjectClassAssignment
    | parameterizedObjectAssignment
    | parameterizedObjectSetAssignment
    )
  
  // ASN1D 17.2.2<3>
  def parameterizedTypeAssignment =
    ( typeReference
    ~ parameterList
    ~ op("::=")
    ~ _type
    ) ^^ { case tr ~ pl ~ _ ~ t => ParameterizedTypeAssignment(tr, pl, t) }

  // ASN1D 17.2.2<5>
  def parameterizedValueAssignment =
    ( valueReference
    ~ parameterList
    ~ _type
    ~ op("::=")
    ~ value
    ) ^^ { case vr ~ pl ~ t ~ _ ~ v => ParameterizedValueAssignment(vr, pl, t, v) }

  // ASN1D 17.2.2<6>
  def parameterizedValueSetTypeAssignment =
    ( typeReference
    ~ parameterList
    ~ _type
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
    ( (paramGovernor <~ op(":")).?
    ~ dummyReference
    ) ^^ { case pg ~ dr => Parameter(pg, dr) }
  
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
  def simpleDefinedType: Parser[SimpleDefinedType] =
    ( externalTypeReference
    | typeReference
    )
  
  // ASN1D 17.2.2<27>
  def parameterizedValue =
    ( simpleDefinedValue ~ actualParameterList
    ) ^^ { case sdv ~ apl => ParameterizedValue(sdv, apl) }
  
  def simpleDefinedValue: Parser[SimpleDefinedValue] =
    ( externalValueReference
    | valueReference
    )
  
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
  
  def actualParameter: Parser[ActualParameter] =
    ( _type
    | value
    | valueSet
    | definedObjectClass
    | object_
    | objectSet
    )

  // Custom
  def default[T](p: Parser[T]) =
    ( kwDefault ~> p
    ) ^^ { v => Default(v) }
    
  // Keywords
  def kwAbsent = kw("ABSENT") ^^ { _ => Absent }
  def kwAbstractSyntax = kw("ABSTRACT-SYNTAX") ^^ { _ => ABSTRACT_SYNTAX }
  def kwApplication = kw("APPLICATION") ^^ { _ => Application }
  def kwAutomatic = kw("AUTOMATIC") ^^ { _ => Automatic }
  def kwBegin = kw("BEGIN")
  def kwBMPString = kw("BMPString") ^^ { _ => BMPString }
  def kwBoolean = kw("BOOLEAN") ^^ { _ => BOOLEAN }
  def kwDefault = kw("DEFAULT")
  def kwDefinitions = kw("DEFINITIONS")
  def kwEnd = kw("END")
  def kwEnumerated = kw("ENUMERATED") ^^ { _ => ENUMERATED }
  def kwExplicit = kw("EXPLICIT") ^^ { _ => Explicit }
  def kwExports = kw("EXPORTS")
  def kwExternal = kw("EXTERNAL") ^^ { _ => EXTERNAL }
  def kwExtensibility = kw("EXTENSIBILITY")
  def kwFalse = kw("FALSE") ^^ { _ => FALSE }
  def kwFrom = kw("FROM")
  def kwGeneralString = kw("GeneralString") ^^ { _ => GeneralString }
  def kwGeneralizedTime = kw("GeneralizedTime") ^^ { _ => GeneralizedTime }
  def kwGraphicString = kw("GraphicString") ^^ { _ => GraphicString }
  def kwIA5String = kw("IA5String") ^^ { _ => IA5String }
  def kwIncludes = kw("INCLUDES")
  def kwImplicit = kw("IMPLICIT") ^^ { _ => Implicit }
  def kwImplied = kw("IMPLIED")
  def kwImports = kw("IMPORTS")
  def kwInteger = kw("INTEGER") ^^ { _ => INTEGER }
  def kwISO646String = kw("ISO646String") ^^ { _ => ISO646String }
  def kwMax = kw("MAX") ^^ { _ => Max }
  def kwMin = kw("MIN") ^^ { _ => Min }
  def kwMinusInfinity = kw("MINUS-INFINITY")  ^^ { _ => MINUS_INFINITY }
  def kwNull = kw("NULL") ^^ { _ => NULL }
  def kwNumericString = kw("NumericString") ^^ { _ => NumericString }
  def kwObjectDescriptor = kw("ObjectDescriptor") ^^ { _ => ObjectDescriptor }
  def kwOptional = kw("OPTIONAL") ^^ { _ => Optional }
  def kwPlusInfinity = kw("PLUS-INFINITY") ^^ { _ => PLUS_INFINITY }
  def kwPresent = kw("PRESENT") ^^ { _ => Present }
  def kwPrintableString = kw("PrintableString") ^^ { _ => PrintableString }
  def kwPrivate = kw("PRIVATE") ^^ { _ => Private }
  def kwReal = kw("REAL") ^^ { _ => REAL }
  def kwRelativeOid = kw("RELATIVE-OID") ^^ { _ => RelativeOidType }
  def kwSequence = kw("SEQUENCE") ^^ { _ => Sequence }
  def kwSet = kw("SET") ^^ { _ => Set }
  def kwT61String = kw("T61String") ^^ { _ => T61String }
  def kwTeletexString = kw("TeletexString") ^^ { _ => TeletexString }
  def kwTrue = kw("TRUE") ^^ { _ => TRUE }
  def kwTypeIdentifier = kw("TYPE-IDENTIFIER") ^^ { _ => TYPE_IDENTIFIER }
  def kwUTF8String = kw("UTF8String") ^^ { _ => UTF8String }
  def kwUnique = kw("UNIQUE") ^^ { _ => UNIQUE }
  def kwUniversal = kw("UNIVERSAL") ^^ { _ => Universal }
  def kwUniversalString = kw("UniversalString") ^^ { _ => UniversalString }
  def kwVideotexString = kw("VideotexString") ^^ { _ => VideotexString }
  def kwVisibleString = kw("VisibleString") ^^ { _ => VisibleString }
  def kwUTCTime = kw("UTCTime") ^^ { _ => UTCTime }
}
