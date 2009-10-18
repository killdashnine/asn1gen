package org.asn1gen.parsing.asn1

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

import org.asn1gen.parsing.asn1.ast._
import org.asn1gen.parsing.syntax._

class Parser extends TokenParsers with ImplicitConversions {
  type Tokens = Lexer
  
  /** A parser which matches a numeric literal */
  def numericLit: Parser[String] = 
    elem("number", _.isInstanceOf[lexical.NumberLit]) ^^ (_.chars)

  /** A parser which matches a string literal */
  def stringLit: Parser[String] = 
    elem("string literal", _.isInstanceOf[lexical.StringLit]) ^^ (_.chars)


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
  
  def root = moduleDefinition
  
  def moduleDefinition = typeReference
    /*( typeReference
    ~ lexical.Keyword("DEFINITIONS")
    ~ tagDefault
    ) ^^ { case tr ~ _ ~ td => ModuleDefinition(ModuleReference2(tr.name)) }*/
  
  def tagDefault = lexical.Keyword("AUTOMATIC") ~ lexical.Keyword("TAGS")
  
  // ASN1D 8.3.2<1-2>
  def bstring = accept("bstring", {case lexical.BString(s) => BString(s)})
  
  // ASN1D 8.3.2<3>
  // TODO: unused
  def comment = accept("comment", {case lexical.CommentLit(n) => n} )
  
  // ASN1D: 8.2.3<4-5>
  // TODO: not implemented
  
  // ASN1D: 8.2.3<6-8>
  def cstring = accept("cstring", {case lexical.CString(s) => CString(s)})
  
  // ASN1D: 8.2.3<9>
  // TODO: not implemented

  def typeReference = elem("type reference", { case lexical.Identifier(n) => n.first.isUpperCase})
}
