import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Lexer
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1 {
  class TestLexer extends TestCase {
    @Test def testOneLineComment() {
      def lexer = new Lexer();
      def charseq1 = new CharSequenceReader("--abc-- a\n");
      assertEquals("abc", lexer.oneLineComment(charseq1).get.comment);
    }
    
    @Test def testMultiLineComment() {
      def lexer = new Lexer();
      def charseq3 = new CharSequenceReader("/* /*Hello*/ world */ ");
      assertEquals(" /*Hello*/ world ", lexer.multiLineComment(charseq3).get.comment);
    }

    @Test def testIdentifier() {
      def lexer = new Lexer();
      def charseq2 = new CharSequenceReader("idenT-IFIER-");
      assertEquals("idenT-IFIER", lexer.identifier(charseq2).get.name);
    }
  }
}
