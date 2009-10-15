package org.asn1gen.parsing.asn1

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input.CharArrayReader.EofCh

/**
 * @author John Ky <"newhoggy"+@+"gmail"+"."+"com">
 */
class Lexer extends StdLexical with ImplicitConversions {
  override def token: Parser[Token] = lowerId | upperId
    //( '\"' ~ rep(charSeq | letter) ~ '\"' ^^ lift(StringLit)
    /*( string ^^ StringLit
    | number ~ letter ^^ { case n ~ l => ErrorToken("Invalid number format : " + n + l) }
    | '-' ~> whitespace ~ number ~ letter ^^ { case ws ~ num ~ l => ErrorToken("Invalid number format : -" + num + l) }
    | '-' ~> whitespace ~ number ^^ { case ws ~ num => NumericLit("-" + num) }
    | number ^^ NumericLit
    | EofCh ^^^ EOF
    | delim
    | '\"' ~> failure("Unterminated string")
    | rep(letter) ^^ checkKeyword
    | failure("Illegal character")
    )*/

  case class CommentLit(chars: String) extends Token {
    override def toString = "`" + chars + "'"
    def comment = chars
  }
  
  abstract case class Id(chars: String) extends Token {
    override def toString =  chars
    def name = chars
  }

  case class LowerId(override val chars: String) extends Id(chars) {
  }

  case class UpperId(override val chars: String) extends Id(chars) {
  }

  def upper : Parser[Elem] = elem("uppercase letter", c => c >= 'A' && c <= 'Z')
  def lower : Parser[Elem] = elem("lowercase letter", c => c >= 'a' && c <= 'z')
  def hyphen : Parser[Elem] = elem("hyphen", c => c == '-')
  def lf : Parser[Elem] = elem("linefeed", c => c == '\n')
  def cr : Parser[Elem] = elem("carriage return", c => c == '\r')
  def endln = ((cr ~ lf) | cr | lf)
  def anychar : Parser[Elem] = elem("any character", c => true)
  def space = elem("space", c => c == ' ')
  def tab = elem("space", c => c == ' ')
  def slash = elem("space", c => c == '/')
  def asterisk = elem("space", c => c == '*')
  
  
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
  
  // 11.2
  def typeReference =
    ( lower
    ~ ( ( letter
        | digit
        | hyphen <~ before(letter | digit)
        ).* ^^ { m => ("" /: m)(_ + _) }
      ).?
    ) ^^ { case c ~ cs => (if (cs.isEmpty) c else "" + c + cs.get) }

  // 11.3
  def identifier =
    ( lower
    ~ ( ( letter
        | digit
        | hyphen <~ before(letter | digit)
        ).* ^^ { m => ("" /: m)(_ + _) }
      ).?
    ) ^^ { case c ~ cs => LowerId("" + c + cs.getOrElse("")) }
  
  // 11.4
  def valueReference = identifier
  
  // 11.5
  def moduleReference = typeReference
  
  // 11.6
  def oneLineCommentMarker = hyphen ~ hyphen
  
  def oneLineCommentRemainder : Parser[String] =
    ( oneLineCommentMarker ^^ { case m => "" }
    | endln ^^ { case m => "" }
    | (anychar ~ oneLineCommentRemainder) ^^ { case c ~ cs => c + cs }
    )
  
  def oneLineComment =
    ( oneLineCommentMarker
    ~ oneLineCommentRemainder
    ) ^^ { case m ~ text =>
      CommentLit(text)
    }
  
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

  override def whitespace : Parser[CommentLit] =
    ( whitespaceChar ^^ { m => CommentLit("") }
    | oneLineComment
    | multiLineComment
    ).* ^^ { cs => CommentLit(("" /: cs)(_ + _)) }
}
