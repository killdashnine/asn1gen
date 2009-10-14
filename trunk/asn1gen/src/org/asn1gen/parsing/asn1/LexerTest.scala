package org.asn1gen.parsing.asn1

import scala.util.parsing.input._

object LexerTest {
  def main(args : Array[String]) : Unit = {
    def lexer = new Lexer();
    def charseq1 = new CharSequenceReader("--abc-- a\n");
    println(lexer.oneLineComment(charseq1));
    def charseq2 = new CharSequenceReader("idenT-IFIER-");
    println(lexer.identifier(charseq2));
    def charseq3 = new CharSequenceReader("/* /*Hello*/ world */ ");
    println(lexer.multiLineComment(charseq3));
  }
}
