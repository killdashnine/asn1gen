package org.asn1gen.parsing.asn1

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.syntax._
import collection.mutable.HashSet
import org.asn1gen.parsing.syntax._
import org.asn1gen.extra.Extras

class Lexer extends Lexical with ImplicitConversions with Asn1Tokens with Extras {
  // see `token' in `Scanners'
  override def token: Parser[Token] =
    ( number
    | identifier
    | ampIdentifier
    | operator
    ) ^^ {
      case t : Asn1Token => t.prevComment = lastComment; t
      case t => t 
    }

  // see `whitespace in `Scanners'
  override def whitespace : Parser[CommentLit] =
    ( whitespaceChar ^^ { m => CommentLit("") }
    | oneLineComment
    | multiLineComment
    ).* ^^ { cs => CommentLit(("" /: cs)(_ + _)) }

  protected def comment: Parser[Any] = (
      '*' ~ '/'  ^^ { _ => ' ' }
    | chrExcept(EofCh) ~ comment
    )
  
  def operator_char = elem("operator", c => c == ':' || c == '=')
  
  def literal(s: String): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = offset
      var i = 0
      var j = start
      while (i < s.length && j < source.length && s.charAt(i) == source.charAt(j)) {
        i += 1
        j += 1
      }
      if (i == s.length)
        Success(source.subSequence(start, j).toString, in.drop(j - offset))
      else 
        Failure("`"+s+"' expected but `"+in.first+"' found", in.drop(start - offset))
    }
  }

  def operator =
    ( literal("...")
    | literal("::=")
    | literal("..")
    | literal("@.")
    | literal("[[")
    | literal("]]")
    | literal("!")
    | literal("(")
    | literal(")")
    | literal(",")
    | literal(".")
    | literal(":")      
    | literal(";")
    | literal("<")
    | literal("[")
    | literal("]")
    | literal("^")
    | literal("{")
    | literal("|")
    | literal("}")
    ) ^^ { cs => Operator(cs) }

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

  def char(c : RandomAccessSeq.Projection[Char]) = elem(
    "'" + c.first + "' to '" + c.last + "' ", c.contains(_))
  def upper : Parser[Elem] = elem("uppercase letter", c => 'A' <= c && c <= 'Z')
  def lower : Parser[Elem] = elem("lowercase letter", c => 'a' <= c && c <= 'z')
  def hyphen : Parser[Elem] = elem("hyphen", _ == '-')
  def lf : Parser[Elem] = elem("linefeed", _ == '\n')
  def cr : Parser[Elem] = elem("carriage return", _ == '\r')
  def endln = ((cr ~ lf) | cr | lf)
  def anychar : Parser[Elem] = elem("any character", c => true)
  def space = elem("space", _ == ' ')
  def tab = elem("space", _ == '\t')
  def slash = elem("space", _ == '/')
  def asterisk = elem("space", _ == '*')
  def dquote = elem("single quote", _ == '"')
  def squote = elem("single quote", _ == '\'')
  def char(c : Char) = elem("'" + c + "'", _ == c)
  def bin_digit = elem("0", _.isBinDigit)
  def not_char(c : Char) = elem("not " + c.toString, _ != c)
  def upper_hex_digit = elem("hexadecimal digit", c => c.isUpperHexDigit)
  def before[T](p: => Parser[T]): Parser[Unit] = not(not(p))
  def ampersand = elem("ampersand", _ == '&') ^^ { _ => Operator("&") }
  
  // ASN1D 8.3.2<1-2>
  def bstring_char =
    ( bin_digit | space | tab | lf | cr )
  
  // ASN1D 8.3.2<1-2>
  def bstring =
    ( squote
    ~ bstring_char.*
    ~ squote
    ~ char('b')
    ) ^^ { case _ ~ data ~ _ ~ _ =>
      BString(data.filter(_.isBinDigit).mkString)
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

  // ASN1D 8.3.2<9>
  // Not implemented
  
  // ASN1D 8.3.2<10-11>
  def hstring_char =
    ( upper_hex_digit | space | tab | lf | cr )
  
  // ASN1D 8.3.2<10-11>
  def hstring =
    ( squote
    ~ hstring_char.*
    ~ squote
    ~ char('H')
    ) ^^ { case _ ~ data ~ _ ~ _ =>
      HString(data.filter(_.isUpperHexDigit).mkString)
    }
  
  // ASN1D 8.3.2<12-15>
  def identifer =
    ( lower
    ~ ( ( letter
        | digit
        | hyphen <~ before(letter | digit)
        ).* ^^ { m => ("" /: m)(_ + _) }
      ).?
    ) ^^ { case c ~ cs =>
      println()
      Identifier("" + c + cs.getOrElse(""))
    }

  // ASN1D 8.3.2<16>
  // Not implemented

  // ASN1D 8.3.2<17>
  // ???
  
  // ASN1D 8.3.2<18>
  def number =
    ( ( char('0')
      ~ not(char('0' to '9'))
      ) ^^ { _ => "0" }
    | ( char('1' to '9')
      ~ char('0' to '9').*
      ) ^^ { case x ~ xs => x + xs.mkString }
    ) ^^ { n => Number(n) }
  
  // ASN1D 8.3.2<19>
  // ???

  // ASN1D 8.3.2<20>
  def objectfieldreference =
    ( ampersand
    ~ objectreference
    )
  
  // ASN1D 8.3.2<21>
  def objectreference = valuereference
  
  // ASN1D 8.3.2<22>
  // ???
  
  // ASN1D 8.3.2<23>
  // ???
  
  // ASN1D 8.3.2<24>
  def signednumber =
    ( number
    | char('-') ~ not(char('0')) ~ number
    )
  
  // ASN1D 8.3.2<25>
  // ???

  // ASN1D 8.3.2<26,29>
  // ???

  // ASN1D 8.3.2<27-30>
  // Not implemented
  
  // ASN1D 8.3.2<31>
  def valuefieldreference =
    ( char('&')
    ~ valuereference
    )
    
  // ASN1D 8.3.2<32>
  def valuereference = identifier
  
  // ASN1D 8.3.2<33>
  // ???

  // ASN1D 8.3.2<34>
  // ???
    
  // ASN1D 8.3.2<35-37>
  // Not implemented
    
  // 11.3
  def identifier_string =
    ( letter
    ~ ( ( letter
        | digit
        | hyphen <~ before(letter | digit)
        ).* ^^ { m => ("" /: m)(_ + _) }
      ).?
    ) ^^ { case c ~ cs =>
      c + cs.getOrElse("");
    }
  
  def identifier = identifier_string ^^ { name =>
    if (reserved contains name) Keyword(name) else Identifier(name)
  }

  
  def ampIdentifier = (char('&') ~> identifier_string) ^^ { name =>
    if (reserved contains name) Keyword(name) else error("keyword not allowed")
  }
      
  // 11.6
  
  var lastComment : String = ""
  
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
    multiLineCommentBegin ~> multiLineCommentRemainder ^^ { comment =>
      lastComment = comment.comment
      comment
    }
}
