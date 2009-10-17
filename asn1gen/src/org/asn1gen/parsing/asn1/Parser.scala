package org.asn1gen.parsing.asn1

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

import org.asn1gen.parsing.asn1.ast._
import org.asn1gen.parsing.syntax._

class Parser extends TokenParsers with ImplicitConversions with Asn1Tokens{
  type Tokens = Lexer

  //import lexical.{Keyword, NumericLit, StringLit, Identifier}

  /** A parser which matches a numeric literal */
  def numericLit: Parser[String] = 
    elem("number", _.isInstanceOf[NumberLit]) ^^ (_.chars)

  /** A parser which matches a string literal */
  def stringLit: Parser[String] = 
    elem("string literal", _.isInstanceOf[StringLit]) ^^ (_.chars)


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
  
  def moduleDefinition =
    ( typeReference
    ~ lexical.Keyword("DEFINITIONS")
    ~ tagDefault
    ) ^^ { case tr ~ _ ~ td => ModuleDefinition(ModuleReference2(tr.name)) }
  
  def tagDefault = lexical.Keyword("AUTOMATIC") ~ lexical.Keyword("TAGS")
  
  def comment = accept("comment", { case lexical.CommentLit(n) => n } )
  
  def typeReference = accept("type reference", {
    case lexical.UpperId(n) => ModuleReference2(n)
  })
}
