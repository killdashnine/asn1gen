package org.asn1gen.parsing.syntax

import scala.util.parsing.syntax.Tokens

/**
 * This component provides the standard `Token's for a simple, Scala-like language. 
 *
 * @author Martin Odersky, Adriaan Moors
 */
trait Asn1Tokens extends Tokens {
  abstract class Asn1Token extends Token {
    var prevComment: String = ""
  }
  
  case class Operator(chars: String) extends Asn1Token {
  }
  
  /** The class of numeric literal tokens */
  case class CommentLit(chars: String) extends Asn1Token {
    override def toString = chars
    def comment = chars
  }

  case class BString(chars: String) extends Asn1Token {
    override def toString = chars
    def string = chars
  }

  case class HString(chars: String) extends Asn1Token {
    override def toString = chars
    def string = chars
  }

  case class CString(chars: String) extends Asn1Token {
    override def toString = chars
    def string = chars
  }

  case class Number(chars: String) extends Asn1Token {
    override def toString = chars
  }

  case class Identifier(chars: String, idReserved: Boolean, wordReserved: Boolean) extends Asn1Token {
    override def toString = "identifier "+chars
    def name = chars
  }

  case class AmpIdentifier(chars: String) extends Asn1Token {
    override def toString = "identifier "+chars
    def name = chars
  }
}
