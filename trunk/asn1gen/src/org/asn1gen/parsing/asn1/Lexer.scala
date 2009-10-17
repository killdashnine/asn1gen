package org.asn1gen.parsing.asn1

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.syntax._
import collection.mutable.HashSet
import org.asn1gen.parsing.syntax._

class Lexer extends Lexical with ImplicitConversions with Asn1Tokens {
  // see `token' in `Scanners'
  override def token: Parser[Token] =
    ( lowerId ^^ { m => if (reserved contains m.name) Keyword(m.name) else m }
    | upperId ^^ { m => if (reserved contains m.name) Keyword(m.name) else m }
    )

  // see `whitespace in `Scanners'
  override def whitespace : Parser[CommentLit] =
    ( whitespaceChar ^^ { m => CommentLit("") }
    | oneLineComment
    | multiLineComment
    ).* ^^ { cs => CommentLit(("" /: cs)(_ + _)) }

  protected def comment: Parser[Any] = (
      '*' ~ '/'  ^^ { case _ => ' '  }
    | chrExcept(EofCh) ~ comment
    )

  /** The set of reserved identifiers: these will be returned as `Keyword's */
  val reserved = new HashSet[String]

  /** The set of delimiters (ordering does not matter) */
  val delimiters = new HashSet[String]

  private var _delim: Parser[Token] = null
  protected def delim: Parser[Token] = {
    if (_delim eq null) { // construct parser for delimiters by |'ing together the parsers for the individual delimiters, 
    // starting with the longest one (hence the sort + reverse) -- otherwise a delimiter D will never be matched if 
    // there is another delimiter that is a prefix of D   
      def parseDelim(s: String): Parser[Token] = accept(s.toList) ^^ { x => Keyword(s) }
      
      val d = new Array[String](delimiters.size)
      delimiters.copyToArray(d,0)
      scala.util.Sorting.quickSort(d) 
      _delim = d.toList.reverse.map(parseDelim).reduceRight[Parser[Token]](_ | _) // no offence :-)      
    }
    
    _delim
  }

  private def lift[T](f: String => T)(xs: List[Char]): T = f(xs.mkString("", "", ""))

  private def lift2[T](f: String => T)(p: ~[Char, List[Char]]): T = lift(f)(p._1 :: p._2)

  def upper : Parser[Elem] = elem("uppercase letter", c => c >= 'A' && c <= 'Z')
  def lower : Parser[Elem] = elem("lowercase letter", c => c >= 'a' && c <= 'z')
  def hyphen : Parser[Elem] = elem("hyphen", c => c == '-')
  def lf : Parser[Elem] = elem("linefeed", c => c == '\n')
  def cr : Parser[Elem] = elem("carriage return", c => c == '\r')
  def endln = ((cr ~ lf) | cr | lf)
  def anychar : Parser[Elem] = elem("any character", c => true)
  def space = elem("space", c => c == ' ')
  def tab = elem("space", c => c == '\t')
  def slash = elem("space", c => c == '/')
  def asterisk = elem("space", c => c == '*')
  def dquote = elem("single quote", c => c == '"')
  def squote = elem("single quote", c => c == '\'')
  def char_b = elem("b", c => c == 'b')
  def char_0 = elem("0", c => c == '0')
  def char_1 = elem("1", c => c == '1')
  def not_char(ch : Char) = elem(ch.toString, c => c != ch)
  
  
  // ASN1D 8.3.2<1-2>
  def bstring_char =
    ( char_0 | char_1 | space | tab | lf | cr )
  
  // ASN1D 8.3.2<1-2>
  def bstring =
    ( squote
    ~ bstring_char.*
    ~ squote
    ~ char_b
    ) ^^ { case _ ~ data ~ _ ~ _ =>
      BString(data.filter(c => c == '0' || c == '1').mkString)
    }
  
  // ASN1D 8.3.2<3>
  def oneLineCommentMarker = hyphen ~ hyphen
  
  // ASN1D 8.3.2<3>
  def oneLineCommentRemainder : Parser[String] =
    ( oneLineCommentMarker ^^ { case m => "" }
    | endln ^^ { case m => "" }
    | (anychar ~ oneLineCommentRemainder) ^^ { case c ~ cs => c + cs }
    )
  
  // ASN1D 8.3.2<3>
  def oneLineComment =
    ( oneLineCommentMarker
    ~ oneLineCommentRemainder
    ) ^^ { case m ~ text =>
      CommentLit(text)
    }

  // ASN1D 8.3.2<4-5>
  // Not implemented

  // ASN1D 8.3.2<4-5>
  def cstring_char =
    ( (dquote ~ dquote) ^^ { _ => '"' }
    | not_char('"')
    )

  // ASN1D 8.3.2<6-8>
  def cstring =
    ( dquote
    ~ cstring_char.*
    ~ dquote
    ) ^^ { case _ ~ data ~ _=>
      CString(data.mkString.lines.map(_.trim).mkString)
    }

  // ASN1D 8.3.2<7>
  
  // ASN1
  def before[T](p: => Parser[T]): Parser[Unit] = not(not(p))
  
  // Can be a typereference or identifier
  def lowerId =
    ( lower
    ~ ( ( letter
        | digit
        | hyphen <~ before(letter | digit)
        ).* ^^ { m => ("" /: m)(_ + _) }
      ).?
    ) ^^ { case c ~ cs => LowerId("" + c + cs.getOrElse("")) }

  def upperId =
    ( upper
    ~ ( ( letter
        | digit
        | hyphen <~ before(letter | digit)
        ).* ^^ { m => ("" /: m)(_ + _) }
      ).?
    ) ^^ { case c ~ cs => UpperId("" + c + cs.getOrElse("")) }
  
  // 11.3
  def identifier =
    ( lower
    ~ ( ( letter
        | digit
        | hyphen <~ before(letter | digit)
        ).* ^^ { m => ("" /: m)(_ + _) }
      ).?
    ) ^^ { case c ~ cs => LowerId(c + cs.getOrElse("")) }
  
  // 11.6
  
  def multiLineCommentBegin = slash ~ asterisk
  def multiLineCommentEnd = asterisk ~ slash
  
  def multiLineCommentRemainder : Parser[CommentLit] =
    ( multiLineCommentEnd ^^ { m => CommentLit("") }
    | ( multiLineComment.?
      ~ anychar
      ~ multiLineCommentRemainder
      ) ^^ {
        case Some(m) ~ c ~ r => {CommentLit("/*" + m.comment + "*/" + c + r.comment)}
        case None ~ c ~ r => {CommentLit(c + r.comment)}
      }
    )
  
  def multiLineComment : Parser[CommentLit] =
    multiLineCommentBegin ~> multiLineCommentRemainder
}
