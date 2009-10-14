package org.asn1gen.parsing.asn1

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

class Parser extends StdTokenParsers with ImplicitConversions {
  def modulereference = "_modulereference_"

  // Fill in abstract defs
  type Tokens = Lexer
  val lexical = new Tokens

  def Root = Comment
  
  def Comment = accept("comment", { case lexical.CommentLit(n) => n } )
}
