package org.asn1gen.parsing.syntax

import scala.util.parsing.syntax.Tokens

/**
 * This component provides the standard `Token's for a simple, Scala-like language. 
 *
 * @author Martin Odersky, Adriaan Moors
 */
trait Asn1Tokens extends Tokens {
  /** The class of keyword tokens */
  case class Keyword(chars: String) extends Token {
    override def toString = "`"+chars+"'"
  }

  /** The class of numeric literal tokens */
  case class CommentLit(chars: String) extends Token {
    override def toString = chars
    def comment = chars
  }

  case class BString(chars: String) extends Token {
    override def toString = chars
    def string = chars
  }

  case class HString(chars: String) extends Token {
    override def toString = chars
    def string = chars
  }

  case class CString(chars: String) extends Token {
    override def toString = chars
    def string = chars
  }

  /** The class of numeric literal tokens */
  case class NumberLit(chars: String) extends Token {
    override def toString = chars
  }

  /** The class of string literal tokens */
  case class StringLit(chars: String) extends Token {
    override def toString = "\""+chars+"\""
  }    

  /** The class of identifier tokens */
  case class Identifier(chars: String) extends Token {
    override def toString = "identifier "+chars
    def name = chars
  }

  case class AmpIdentifier(chars: String) extends Token {
    override def toString = "identifier "+chars
    def name = chars
  }
}
