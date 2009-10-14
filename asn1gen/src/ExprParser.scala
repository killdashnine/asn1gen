import scala.util.parsing.combinator.syntactical._

object ExprParser extends StandardTokenParsers {
  // Lexicals
  def typereference = "_typereference_"
  def identifier = "_identifier_"
  def valuereference = "_valuereference_"
  def modulereference = "_modulereference_"
  def comment = "_comment_"
  def number = "_number_"
  def realnumber = "_realnumber_"
  def bstring = "_bstring_"
  def xmlbstring = "_xmlbstring_"
  def hstring = "_hstring_"
  def xmlhstring = "_xmlhstring_"
  def cstring = "_cstring_"
  def xmlcstring = "_xmlcstring_"
  def xmlasn1typename = "_xmlasn1typename_"

  def ABSENT = "ABSENT"
  def ABSTRACT_SYNTAX = "ABSTRACT-SYNTAX"
  def ALL = "ALL"
  def APPLICATION = "APPLICATION"
  def AUTOMATIC = "AUTOMATIC"
  def BEGIN = "BEGIN"
  def BIT = "BIT"
  def BMPString = "BMPString"
  def BOOLEAN = "BOOLEAN"
  def BY = "BY"
  def CHARACTER = "CHARACTER"
  def CHOICE = "CHOICE"
  def CLASS = "CLASS"
  def COMPONENT = "COMPONENT"
  def COMPONENTS = "COMPONENTS"
  def CONSTRAINED = "CONSTRAINED"
  def CONTAINING = "CONTAINING"
  def DEFAULT = "DEFAULT"
  def DEFINITIONS = "DEFINITIONS"
  def EMBEDDED = "EMBEDDED"
  def ENCODED = "ENCODED"
  def END = "END"
  def ENUMERATED = "ENUMERATED"
  def EXCEPT = "EXCEPT"
  def EXPLICIT = "EXPLICIT"
  def EXPORTS = "EXPORTS"
  def EXTENSIBILITY = "EXTENSIBILITY"
  def EXTERNAL = "EXTERNAL"
  def FALSE = "FALSE"
  def FROM = "FROM"
  def GeneralizedTime = "GeneralizedTime"
  def GeneralString = "GeneralString"
  def GraphicString = "GraphicString"
  def IA5String = "IA5String"
  def IDENTIFIER = "IDENTIFIER"
  def IMPLICIT = "IMPLICIT"
  def IMPLIED = "IMPLIED"
  def IMPORTS = "IMPORTS"
  def INCLUDES = "INCLUDES"
  def INSTANCE = "INSTANCE"
  def INTEGER = "INTEGER"
  def INTERSECTION = "INTERSECTION"
  def ISO646String = "ISO646String"
  def MAX = "MAX"
  def MIN = "MIN"
  def MINUS_INFINITY = "MINUS-INFINITY"
  def NULL = "NULL"
  def NumericString = "NumericString"
  def OBJECT = "OBJECT"
  def ObjectDescriptor = "ObjectDescriptor"
  def OCTET = "OCTET"
  def OF = "OF"
  def OPTIONAL = "OPTIONAL"
  def PATTERN = "PATTERN"
  def PDV = "PDV"
  def PLUS_INFINITY = "PLUS-INFINITY"
  def PRESENT = "PRESENT"
  def PrintableString = "PrintableString"
  def PRIVATE = "PRIVATE"
  def REAL = "REAL"
  def RELATIVE_OID = "RELATIVE-OID"
  def SEQUENCE = "SEQUENCE"
  def SET = "SET"
  def SIZE = "SIZE"
  def STRING = "STRING"
  def SYNTAX = "SYNTAX"
  def T61String = "T61String"
  def TAGS = "TAGS"
  def TeletexString = "TeletexString"
  def TRUE = "TRUE"
  def TYPE_IDENTIFIER = "TYPE-IDENTIFIER"
  def UNION = "UNION"
  def UNIQUE = "UNIQUE"
  def UNIVERSAL = "UNIVERSAL"
  def UniversalString = "UniversalString"
  def UTCTime = "UTCTime"
  def UTF8String = "UTF8String"
  def VideotexString = "VideotexString"
  def VisibleString = "VisibleString"
  def WITH = "WITH"
  def empty = ""


  // 12. Module definition
  
  def ModuleDefinition =
    ( ModuleIdentifier
    ~ DEFINITIONS
    ~ TagDefault
    ~ ExtensionDefault
    ~ "::="
    ~ BEGIN
    ~ ModuleBody
    ~ END
    )

  def ModuleIdentifier =
    ( modulereference
    ~ DefinitiveIdentifier
    )

  def DefinitiveIdentifier =
     ( ( "{"
       ~ DefinitiveObjIdComponentList
       ~ "}"
       )
     | empty
     )

  def DefinitiveObjIdComponentList = rep(DefinitiveObjIdComponent)

  def DefinitiveObjIdComponent =
     ( NameForm
     | DefinitiveNumberForm
     | DefinitiveNameAndNumberForm
     )

  def DefinitiveNumberForm = number

  def DefinitiveNameAndNumberForm =
    ( identifier
    ~ "("
    ~ DefinitiveNumberForm
    ~ ")"
    )

  def TagDefault =
    ( (EXPLICIT ~ TAGS)
    | (IMPLICIT ~ TAGS)
    | (AUTOMATIC ~ TAGS)
    | empty
    )

  def ExtensionDefault =
    ( (EXTENSIBILITY ~ IMPLIED)
    | empty
    )

  def ModuleBody =
    ( (Exports ~ Imports ~ AssignmentList)
    | empty
    )

  def Exports =
    ( (EXPORTS ~ SymbolsExported ~ ";")
    | (EXPORTS ~ ALL ~ ";")
    | empty
    )

  def SymbolsExported =
    ( SymbolList
    | empty
    )

  def Imports = (IMPORTS ~ SymbolsImported ~ ";") | empty

  def SymbolsImported = SymbolsFromModuleList | empty

  def SymbolsFromModuleList = rep(SymbolsFromModule)

  def SymbolsFromModule = SymbolList ~ FROM ~ GlobalModuleReference
  
  def GlobalModuleReference = modulereference ~ AssignedIdentifier

  def AssignedIdentifier = ObjectIdentifierValue | DefinedValue |  empty

  def SymbolList = repsep(Symbol, ",")

  def Symbol = Reference //| ParameterizedReference

  def Reference =
    ( typereference
    | valuereference
    //| objectclassreference
    //| objectreference
    //| objectsetreference
    )

  def AssignmentList = rep(Assignment)

  def Assignment = TypeAssignment | ValueAssignment |/* XMLValueAssignment |*/  ValueSetTypeAssignment /*| ObjectClassAssignment | ObjectAssignment |  ObjectSetAssignment | ParameterizedAssignment*/
  
  // 13. Referencing type and value definitions.
  
  def DefinedType =
    ( ExternalTypeReference
    | typereference
    //| ParameterizedType
    //| ParameterizedValueSetType
    )

  def DefinedValue =
    ( ExternalValueReference
    | valuereference
    //| ParameterizedValue
    )
  
  def NonParameterizedTypeName =
    ( ExternalTypeReference
    | typereference
    | xmlasn1typename
    )
  
  def ExternalTypeReference =
    ( modulereference
    ~ "."
    ~ typereference
    )
  
  def ExternalValueReference =
    ( modulereference
    ~ "."
    ~ valuereference
    )

  // 14. Notation to support references to ASN.1 components

  def AbsoluteReference =
    ( "@"
    ~ ModuleIdentifier
    ~ "."
    ~ ItemSpec
    )
  
  def ItemSpec =
    typereference ~ rep ("." ~ ComponentId)
  
  def ItemId = ItemSpec

  def ComponentId =
    ( identifier
    | number
    | "*"
    )

// 15. Assigning types and values.

  def TypeAssignment =
    ( typereference
    ~ "::="
    ~ Type
    )
  
  def ValueAssignment =
    ( valuereference
    | Type ~ "::=" ~ Value
    )
  
  /*def XMLValueAssignment =
    ( valuereference ~ "::=" ~ XMLTypedValue)*/

  /*def XMLTypedValue =
    ( ( "<"
      & NonParameterizedTypeName
      ~ ">"
      ~ XMLValue
      ~ "</"
      & NonParameterizedTypeName
      ~ ">")
    | ("<" & NonParameterizedTypeName ~ "/>")
    )*/

  def ValueSetTypeAssignment = typereference ~ Type ~ "::=" ~ ValueSet
  
  def ValueSet = "{" ~ ElementSetSpecs ~ "}"

  // 16. Definition of types and values.

  def Type = BuiltinType | ReferencedType | ConstrainedType

  def BuiltinType = BitStringType | BooleanType | CharacterStringType | ChoiceType |  EmbeddedPDVType | EnumeratedType |  ExternalType |  /*InstanceOfType |*/  IntegerType | NullType | /* ObjectClassFieldType |*/  ObjectIdentifierType |  OctetStringType | RealType |  RelativeOIDType | SequenceType |  /*SequenceOfType |*/  SetType | SetOfType //| TaggedType
  
  def ReferencedType = DefinedType | UsefulType /*| SelectionType | TypeFromObject |  ValueSetFromObjects*/
  
  def NamedType : Parser[Object] = identifier ~ Type
  
  def Value = BuiltinValue | ReferencedValue// | ObjectClassFieldValue
  /*def XMLValue = XMLBuiltinValue | XMLObjectClassFieldValue*/
  
  def BuiltinValue = BitStringValue | BooleanValue |  CharacterStringValue |  ChoiceValue | /*EmbeddedPDVValue |*/ EnumeratedValue | ExternalValue | /*InstanceOfValue |*/ IntegerValue |  NullValue | ObjectIdentifierValue | OctetStringValue |  RealValue | RelativeOIDValue |  SequenceValue | SequenceOfValue | SetValue |  SetOfValue |  TaggedValue
  
  /*def XMLBuiltinValue =
    ( XMLBitStringValue
    | XMLBooleanValue 
    | XMLCharacterStringValue 
    | XMLChoiceValue 
    | XMLEmbeddedPDVValue
    | XMLEnumeratedValue
    | XMLExternalValue
    | XMLInstanceOfValue
    | XMLIntegerValue
    | XMLNullValue
    | XMLObjectIdentifierValue
    | XMLOctetStringValue
    | XMLRealValue
    | XMLRelativeOIDValue
    | XMLSequenceValue
    | XMLSequenceOfValue
    | XMLSetValue
    | XMLSetOfValue
    | XMLTaggedValue
    )*/
  
  def ReferencedValue = (DefinedValue /*| ValueFromObject*/)
  
  def NamedValue : ExprParser.Parser[ExprParser.~[String,java.lang.Object]] = identifier ~ Value
  /*def XMLNamedValue = "<" & identifier ~ ">" ~ XMLValue ~ "</" & identifier ~ ">"*/

  // 17. Notation for the boolean type.

  def BooleanType = BOOLEAN

  def BooleanValue = TRUE | FALSE

 /* def XMLBooleanValue =
    ( ("<" & "true" ~ "/>")
    | ("<" & "false" ~ "/>")
    )*/
  
  // 18. Notation for the integer type.

  def IntegerType =
    ( INTEGER
    | (INTEGER ~ "{" ~ NamedNumberList ~ "}")
    )
  def NamedNumberList = repsep(NamedNumber, ",")
  
  def NamedNumber =
    ( (identifier ~ "(" ~ SignedNumber ~ ")")
    | (identifier ~ "(" ~ DefinedValue ~ ")")
    )
  def SignedNumber = number | ("-" ~ number)
  
  def IntegerValue = SignedNumber | identifier
  /*def XMLIntegerValue = SignedNumber | ("<" & identifier ~ "/>")*/

  // 19. Notation for the enumerated type

  def EnumeratedType = ENUMERATED ~ "{" ~ Enumerations ~ "}"
  def Enumerations =
    ( RootEnumeration
    | (RootEnumeration ~ "," ~ "..." ~ ExceptionSpec)
    | (RootEnumeration ~ "," ~ "..." ~ ExceptionSpec ~ "," ~ AdditionalEnumeration)
    )
  def RootEnumeration = Enumeration
  def AdditionalEnumeration = Enumeration
  def Enumeration = repsep(EnumerationItem, ",")
  def EnumerationItem = identifier | NamedNumber
  
  def EnumeratedValue = identifier
  /*def XMLEnumeratedValue = "<" & identifier ~ "/>"*/

  // 20. Notation for the real type.

  def RealType = REAL
  def RealValue = NumericRealValue | SpecialRealValue
  def NumericRealValue =
    ( realnumber
    | ("-" ~ realnumber)
    | SequenceValue
    ) // Value of the associated sequence type
  def SpecialRealValue = PLUS_INFINITY | MINUS_INFINITY

  /*def XMLRealValue = XMLNumericRealValue | XMLSpecialRealValue
  def XMLNumericRealValue = realnumber | ("-" ~ realnumber)
  
  def XMLSpecialRealValue = ("<" & PLUS-INFINITY ~ "/>") | ("<" & MINUS-INFINITY ~ "/>")*/

  // 22. Notation for the bitstring type.

  def BitStringType = (BIT ~ STRING) | (BIT ~ STRING ~ "{" ~ NamedBitList ~ "}")
  def NamedBitList = repsep(NamedBit, ",")
  def NamedBit = (identifier ~ "(" ~ number ~ ")") | (identifier ~ "(" ~ DefinedValue ~ ")")

  def BitStringValue : ExprParser.Parser[java.lang.Object] =
    ( bstring
    | hstring
    | ("{" ~ IdentifierList ~ "}")
    | ("{" ~ "}")
    | (CONTAINING ~ Value)
    )
  def IdentifierList = repsep(identifier, ",")
  
  /*def XMLBitStringValue = XMLTypedValue
  def XMLIdentifierList =
    ( ("<" & identifier ~ "/>")
    | (XMLIdentifierList ~ "<" & identifier ~ "/>")
    )*/

  // 22. Notation for the octetstring type.

  def OctetStringType = OCTET ~ STRING
  
  def OctetStringValue : Parser[Object] = bstring | hstring | (CONTAINING ~ Value)
  /*def XMLOctetStringValue = XMLTypedValue | xmlhstring*/
  
  // 23. Notation for the null type.
  
  def NullType = NULL
  def NullValue = NULL
  def XMLNullValue = empty
  
  // 24. Notation for sequence types.
  
  def SequenceType =
    ( (SEQUENCE ~ "{" ~ "}")
    | (SEQUENCE ~ "{" ~ ExtensionAndException ~ OptionalExtensionMarker ~ "}")
    | (SEQUENCE ~ "{" ~ ComponentTypeLists ~ "}")
    )
  def ExtensionAndException = "..." | ("..." ~ ExceptionSpec)
  def OptionalExtensionMarker = ("," ~ "...") | empty
  def ComponentTypeLists =
    ( RootComponentTypeList
    | ( RootComponentTypeList
      ~ ","
      ~ ExtensionAndException
      ~ ExtensionAdditions
      ~ OptionalExtensionMarker
      )
    | ( RootComponentTypeList
      ~ ","
      ~ ExtensionAndException
      ~ ExtensionAdditions
      ~ ExtensionEndMarker
      ~ ","
      ~ RootComponentTypeList
      )
    | ( ExtensionAndException
      ~ ExtensionAdditions
      ~ ExtensionEndMarker
      ~ ","
      ~ RootComponentTypeList
      )
    | ( ExtensionAndException
      ~ ExtensionAdditions
      ~ OptionalExtensionMarker
      )
    )
  
  def RootComponentTypeList = ComponentTypeList
  def ExtensionEndMarker = "," ~ "..."
  def ExtensionAdditions = ("," ~ ExtensionAdditionList) | empty
  def ExtensionAdditionList = repsep(ExtensionAddition, ",")
  def ExtensionAddition = ComponentType | ExtensionAdditionGroup
  def ExtensionAdditionGroup = "[[" ~ VersionNumber ~ ComponentTypeList ~ "]]"
  def VersionNumber = empty | (number ~ ":")
  def ComponentTypeList = repsep(ComponentType, ",")
  def ComponentType : Parser[Object] =
    ( NamedType
    | (NamedType ~ OPTIONAL)
    | (NamedType ~ DEFAULT ~ Value)
    | (COMPONENTS ~ OF ~ Type)
    )
  
  def SequenceValue = ("{" ~ ComponentValueList ~ "}") | ("{" ~ "}")
  def ComponentValueList = repsep(NamedValue, ",")
  /*def XMLSequenceValue = XMLComponentValueList | empty
  def XMLComponentValueList = XMLNamedValue | (XMLComponentValueList ~ XMLNamedValue)*/

  // 25. Notation for sequence-of types.

  def SequenceOfValue = ("{" ~ ValueList ~ "}") | ("{" ~ NamedValueList ~ "}") | ("{" ~ "}")
  def ValueList : Parser[Object] = Value | (ValueList ~ "," ~ Value)
  def NamedValueList = repsep(NamedValue, ",")
  /*def XMLSequenceOfValue = XMLValueList | XMLDelimitedItemList |  XMLSpaceSeparatedList | empty
  def XMLValueList = XMLValueOrEmpty | (XMLValueOrEmpty ~ XMLValueList)
  def XMLValueOrEmpty = XMLValue | ("<" & NonParameterizedTypeName ~ "/>")
  def XMLSpaceSeparatedList = XMLValueOrEmpty | (XMLValueOrEmpty ~ " " ~ XMLSpaceSeparatedList)
  def XMLDelimitedItemList = XMLDelimitedItem | (XMLDelimitedItem ~ XMLDelimitedItemList)
  def XMLDelimitedItem =
    ( ( "<"
      & NonParameterizedTypeName
      ~ ">"
      ~ XMLValue
      ~ "</"
      & NonParameterizedTypeName
      ~ ">"
      )
    | ( "<"
      & identifier
      ~ ">"
      ~ XMLValue
      ~ "</"
      & identifier
      ~ ">"
      )
    )*/

  // 26. Notation for set types.

  def SetType =
    ( ( SET
      ~ "{"
      ~ "}"
      )
    | ( SET
      ~ "{"
      ~ ExtensionAndException
      ~ OptionalExtensionMarker
      ~ "}"
      )
    | ( SET
      ~ "{"
      ~ ComponentTypeLists
      ~ "}"
      )
    )
  def SetValue = ("{" ~ ComponentValueList ~ "}") | ("{" ~ "}")
  /*def XMLSetValue = XMLComponentValueList | empty*/
  
  // 27. Notation for set-of types.
  
  def SetOfType : Parser[Object] = (SET ~ OF ~ Type) | (SET ~ OF ~ NamedType)
  def SetOfValue = ("{" ~ ValueList ~ "}") | ("{" ~ NamedValueList ~ "}") | ("{" ~ "}")
  /*def XMLSetOfValue = XMLValueList | XMLDelimitedItemList | XMLSpaceSeparatedList | empty*/
  
  // 28. Notation for choice types.
  
  def ChoiceType = (CHOICE ~ "{" ~ AlternativeTypeLists ~ "}")
  def AlternativeTypeLists =
    ( RootAlternativeTypeList
    | ( RootAlternativeTypeList
      ~ ","
      ~ ExtensionAndException
      ~ ExtensionAdditionAlternatives
      ~ OptionalExtensionMarker
      )
    )
  def RootAlternativeTypeList = AlternativeTypeList
  def ExtensionAdditionAlternatives = ("," ~ ExtensionAdditionAlternativesList) | empty
  def ExtensionAdditionAlternativesList = repsep(ExtensionAdditionAlternative, ",")
  def ExtensionAdditionAlternative = ExtensionAdditionAlternativesGroup | NamedType
  def ExtensionAdditionAlternativesGroup =
    ("[[" ~ VersionNumber ~ AlternativeTypeList ~ "]]")
  def AlternativeTypeList = repsep(NamedType, ",")
  
  def ChoiceValue : Parser[Object] = (identifier ~ ":" ~ Value)
  /*def XMLChoiceValue = "<" & identifier ~ ">" ~ XMLValue ~ "</" & identifier ~ ">"*/

  // 29. Notation for selection types.

  /*def SelectionType = identifier & "<" & Type*/
  
  // 30. Notation for tagged types.
  
  /*def TaggedType = (Tag & Type) | (Tag & IMPLICIT & Type) | (Tag & EXPLICIT & Type)*/
  /*def Tag = ("[" & Class & ClassNumber & "]")*/
  def ClassNumber = number | DefinedValue
  def Class = UNIVERSAL | APPLICATION | PRIVATE | empty
  
  def TaggedValue : Parser[Object] = Value
  /*def XMLTaggedValue = XMLValue*/

  // 31. Notation for the object identifier type.

  def ObjectIdentifierType = OBJECT ~ IDENTIFIER
  
  def ObjectIdentifierValue =
    ( ("{" ~ ObjIdComponentsList ~ "}")
    | ("{" ~ DefinedValue ~ ObjIdComponentsList ~ "}")
    )
  def ObjIdComponentsList = rep(ObjIdComponents)
  def ObjIdComponents = NameForm | NumberForm |  NameAndNumberForm | DefinedValue
  def NameForm = identifier
  def NumberForm = number | DefinedValue
  def NameAndNumberForm = identifier ~ "(" ~ NumberForm ~ ")"
  /*def XMLObjectIdentifierValue = XMLObjIdComponentList
  def XMLObjIdComponentList = XMLObjIdComponent | XMLObjIdComponent & "." & XMLObjIdComponentList
  def XMLObjIdComponent = NameForm | XMLNumberForm | XMLNameAndNumberForm
  def XMLNumberForm = number*/
  /*def XMLNameAndNumberForm = identifier & "(" & XMLNumberForm & ")"*/

  // 32. Notation for the relative object identifier type.

  def RelativeOIDType = RELATIVE_OID

  def RelativeOIDValue = "{" ~ RelativeOIDComponentsList ~ "}"
  def RelativeOIDComponentsList = rep(RelativeOIDComponents)

  def RelativeOIDComponents = NumberForm | NameAndNumberForm | DefinedValue
  /*def XMLRelativeOIDValue = XMLRelativeOIDComponentList
  def XMLRelativeOIDComponentList =
    ( XMLRelativeOIDComponent
    | (XMLRelativeOIDComponent & "." & XMLRelativeOIDComponentList)
    )
  def XMLRelativeOIDComponent = XMLNumberForm | XMLNameAndNumberForm*/
  
  // 33. Notation for the embedded-pdv type.
  
  def EmbeddedPDVType = EMBEDDED ~ PDV
  
  def EmbeddedPdvValue = SequenceValue  // value of associated type defined in 33.5
  //def XMLEmbeddedPDVValue = XMLSequenceValue  // value of associated type defined in 33.5

  // 34. Notation for the external type.

  def ExternalType = EXTERNAL

  def ExternalValue = SequenceValue // value of associated type defined in 34.5 XMLExternalValue = XMLSequenceValue -- value of associated type defined in 34.5

  // 36. Notation for character string types.

  def CharacterStringType = RestrictedCharacterStringType | UnrestrictedCharacterStringType
  
  def CharacterStringValue = RestrictedCharacterStringValue | UnrestrictedCharacterStringValue
  //def XMLCharacterStringValue = XMLRestrictedCharacterStringValue | XMLUnrestrictedCharacterStringValue
  
  // 37. Definition of restricted character string types.
  
  def RestrictedCharacterStringType =
    ( BMPString
    | GeneralString
    | GraphicString
    | IA5String
    | ISO646String
    | NumericString
    | PrintableString
    | TeletexString
    | T61String
    | UniversalString
    | UTF8String
    | VideotexString
    | VisibleString
    )

  def RestrictedCharacterStringValue = cstring | CharacterStringList | Quadruple | Tuple
  def CharacterStringList = ("{" ~ CharSyms ~ "}")
  def CharSyms = repsep(CharsDefn, ",")
  
  def CharsDefn = (cstring | Quadruple | Tuple | DefinedValue)
  def Quadruple = "{" ~ Group ~ "," ~ Plane ~ "," ~ Row ~ "," ~ Cell ~ "}"
  def Group = number
  def Plane = number
  def Row = number
  def Cell = number
  def Tuple = "{" ~ TableColumn ~ "," ~ TableRow ~ "}"
  def TableColumn = number
  def TableRow = number
  
  def XMLRestrictedCharacterStringValue = xmlcstring

  // 40. Definition of unrestricted character string types.

  def UnrestrictedCharacterStringType = CHARACTER ~ STRING
  
  def UnrestrictedCharacterStringValue = SequenceValue // value of associated type defined in 40.5
  //def XMLUnrestrictedCharacterStringValue = XMLSequenceValue // value of associated type defined in 40.5

  // 41. Notation for types defined in clauses 42 to 44.

  def UsefulType = typereference

  // 42. Generalized time.
  
  // 43. Universal time.
  
  // 44. The object descriptor type.
      
  // 45. Constrained types.
      
  def ConstrainedType : Parser[Object] = (Type ~ Constraint) | TypeWithConstraint
        
  def TypeWithConstraint =
    ( ( SET
      ~ Constraint
      ~ OF
      ~ Type
      )
    | ( SET
      ~ SizeConstraint
      ~ OF
      ~ Type
      )
    | ( SEQUENCE
      ~ Constraint
      ~ OF
      ~ Type
      )
    | ( SEQUENCE
      ~ SizeConstraint
      ~ OF
      ~ Type
      )
    | ( SET
      ~ Constraint
      ~ OF
      ~ NamedType
      )
    | ( SET
      ~ SizeConstraint
      ~ OF
      ~ NamedType
      )
    | ( SEQUENCE
      ~ Constraint
      ~ OF
      ~ NamedType
      )
    | ( SEQUENCE
      ~ SizeConstraint
      ~ OF
      ~ NamedType
      )
    )
      
  def Constraint = "(" ~ ConstraintSpec ~ ExceptionSpec ~ ")"
  def ConstraintSpec = SubtypeConstraint //| GeneralConstraint
  
  def SubtypeConstraint : Parser[Object] = ElementSetSpecs

  // 46. Element set specification.

  def ElementSetSpecs =
    ( RootElementSetSpec
    | ( RootElementSetSpec
      ~ ","
      ~ "..."
      )
    | ( RootElementSetSpec
      ~ ","
      ~ "..."
      ~ ","
      ~ AdditionalElementSetSpec
      )
    )
  def RootElementSetSpec = ElementSetSpec
  def AdditionalElementSetSpec = ElementSetSpec
  def ElementSetSpec = Unions | (ALL ~ Exclusions)
  def Unions = Intersections | (UElems ~ UnionMark ~ Intersections)
  def UElems : Parser[Object] = Unions
  def Intersections = IntersectionElements | (IElems ~ IntersectionMark ~ IntersectionElements)
  def IElems : Parser[Object] = Intersections
  def IntersectionElements = Elements | (Elems ~ Exclusions)
  def Elems = Elements
  def Exclusions = EXCEPT ~ Elements
  def UnionMark = "|" | UNION
  def IntersectionMark = "^" | INTERSECTION

  def Elements : Parser[Object] =
    ( SubtypeElements
    //| ObjectSetElements
    | ( "("
      ~ ElementSetSpec
      ~ ")"
      )
    )

  // 47. Subtype elements.

  def SubtypeElements =
    ( SingleValue
    | ContainedSubtype
    | ValueRange
    | PermittedAlphabet
    | SizeConstraint
    | TypeConstraint
    | InnerTypeConstraints
    | PatternConstraint
    )
  
  def SingleValue = Value

  def ContainedSubtype = Includes ~ Type
  def Includes = INCLUDES | empty

  def ValueRange =
    ( LowerEndpoint
    ~ ".."
    ~ UpperEndpoint
    )
  
  def LowerEndpoint =
    ( LowerEndValue
    | ( LowerEndValue
      ~ "<"
      )
    )
  
  def UpperEndpoint =
    ( UpperEndValue
    | ( "<"
      ~ UpperEndValue
      )
    )
  
  def LowerEndValue = Value | MIN
  def UpperEndValue = Value | MAX
  
  def SizeConstraint = SIZE ~ Constraint
  
  def TypeConstraint = Type

  def PermittedAlphabet = FROM ~ Constraint
  
  def InnerTypeConstraints =
    ( ( WITH
      ~ COMPONENT
      ~ SingleTypeConstraint
      )
    | ( WITH
      ~ COMPONENTS
      ~ MultipleTypeConstraints
      )
    )
  
  def SingleTypeConstraint = Constraint

  def MultipleTypeConstraints =
    ( FullSpecification
    | PartialSpecification
    )
  
  def FullSpecification = "{" ~ TypeConstraints ~ "}"
  
  def PartialSpecification =
    ( "{" ~ "..." ~ "," ~ TypeConstraints ~ "}"
    )

  def TypeConstraints : Parser[Object] =
    ( NamedConstraint
    | ( NamedConstraint
      ~ ","
      ~ TypeConstraints
      )
    )
  
  def NamedConstraint = identifier ~ ComponentConstraint
  
  def ComponentConstraint = ValueConstraint ~ PresenceConstraint
  
  def ValueConstraint = Constraint | empty

  def PresenceConstraint = PRESENT | ABSENT | OPTIONAL | empty

  def PatternConstraint = PATTERN ~ Value
  
  // 48. The extension marker.
  
  // 49. The exception identifier.
  
  def ExceptionSpec =
    ( ( "!"
      ~ ExceptionIdentification
      )
    | empty
    )

  def ExceptionIdentification : Parser[Object] =
    ( SignedNumber
    | DefinedValue
    | ( Type
      ~ ":"
      ~ Value
      )
    )

    /*
    lexical.delimiters ++= List("+")

    def value = numericLit ^^ { s => EConst(s.toDouble) }

    def sum = value ~ "+" ~ value ^^ { case left ~ "+" ~ right =>
            EAdd(left, right) }

    def expr = ( sum | value )          //top level expression
*/
    def parse(s:String) = {
        val tokens = new lexical.Scanner(s)
        phrase(ModuleDefinition)(tokens)
    }

    def apply(s:String):Boolean = {
        parse(s) match {
            case Success(tree, _) => true
            case e: NoSuccess =>
              println(e);
                   throw new IllegalArgumentException("Bad syntax: "+s)
        }
    }

    //Simplify testing
    def test(exprstr: String) = {
        parse(exprstr) match {
            case Success(tree, _) =>
                println("Tree: "+tree)
            case e: NoSuccess => Console.err.println(e)
        }
    }

    //A main method for testing
    def main(args: Array[String]) = {
      test(args(0))
    }
}

