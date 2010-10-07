package org.asn1gen.parsing.asn1

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

import org.asn1gen.parsing.asn1.ast._
import org.asn1gen.parsing.syntax._

class Asn1ParserBase extends TokenParsers with ImplicitConversions {
  type Tokens = Asn1Lexer
  
  val lexical = new Tokens
  
  def elem[U](kind: String)(f: PartialFunction[Elem, U]): Parser[U] =
    elem(kind, {_: Elem => true}) ^? (f, _ => "Expecting " + kind + ".")

  def op(chars: String): Parser[Operator] = elem("operator " + chars) {
    case lexical.Operator(`chars`) => Operator(chars)
  }
  def op(chars1: String, chars2: String): Parser[Operator] =
    ( positioned(op(chars1))
    ~ positioned(op(chars2))
    ) ^^ { case o@(op1 ~ op2) if op1.pos.column + chars1.length == op2.pos.column && op1.pos.line == op2.pos.line =>
      Operator(chars1 + chars2)
    }
  def kw(chars: String) = elem("keyword " + chars) {
    case lexical.Identifier(`chars`, true, _) => Keyword(chars)
  }
  def empty = success("") ^^ { _ => Empty }

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
    case lexical.Identifier(n, false, _) if n.head.isLower => Identifier(n)
  }

  // ASN1D: 8.2.3<18>
  def number = elem("number") {
    case lexical.Number(s) => Number(s)
  }

  // ASN1D: 8.2.3<26>
  /*def typeReference = elem(
    "type reference",
    { case lexical.Identifier(n) => n.first.isUpperCase}) ^^ {
      case lexical.Identifier(n) => TypeReference(n) 
    }*/
  def typeReference = elem("type reference") {
    case lexical.Identifier(n, false, _) if (n.head.isUpper) => TypeReference(n)
  } | failure ("incorrect type reference")

  // ASN1D: 8.2.3<31>
  def valueReference = elem("value reference") {
    case lexical.Identifier(n, false, _) if (n.head.isLower) => ValueReference(n)
  } | failure ("incorrect type reference")

  // ASN1D: 8.2.3<34-35>
  def word = elem("word") {
    case lexical.Identifier(n, _, false) if (n.head.isUpper) => Word(n)
  } | failure ("incorrect type reference")
}