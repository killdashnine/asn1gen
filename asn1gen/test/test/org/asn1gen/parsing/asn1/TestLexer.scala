import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Lexer
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1 {
  class TestLexer extends TestCase {
    val lexer = new Lexer()
    
    @Test def test_bstring_1() {
      def charseq = new CharSequenceReader("''b")
      assertEquals("", lexer.bstring(charseq).get.string)
    }
    
    @Test def test_bstring_2() {
      def charseq = new CharSequenceReader("' 1 0 1\r\n 0  1\t01'b")
      assertEquals("1010101", lexer.bstring(charseq).get.string)
    }
    
    @Test def test_cstring_1() {
      def charseq = new CharSequenceReader("\"abc def\"")
      assertEquals("abc def", lexer.cstring(charseq).get.string)
    }
    
    @Test def test_cstring_2() {
      def charseq = new CharSequenceReader("\"abc\"\"def\"")
      assertEquals("abc\"def", lexer.cstring(charseq).get.string)
    }
    
    @Test def test_cstring_3() {
      def charseq = new CharSequenceReader("\" \n abc \n \"\"def \n \"")
      assertEquals("abc\"def", lexer.cstring(charseq).get.string)
    }
    
    @Test def test_oneLineComment() {
      def charseq1 = new CharSequenceReader("--abc-- a\n");
      assertEquals("abc", lexer.oneLineComment(charseq1).get.comment);
    }
    
    @Test def test_multiLineComment() {
      def charseq3 = new CharSequenceReader("/* /*Hello*/ world */ ");
      assertEquals(" /*Hello*/ world ", lexer.multiLineComment(charseq3).get.comment);
    }

    @Test def test_identifier() {
      def charseq2 = new CharSequenceReader("idenT-IFIER-");
      assertEquals("idenT-IFIER", lexer.identifier(charseq2).get.name);
    }
  }
}
