import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch17 {
  class TestS3S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_01() {
      val text = """
        CharacterString{INTEGER:max-length} ::= CHOICE {
          teletexString TeletexString (SIZE (1..max-length)!exceeds-max-length),
          printableString PrintableString (SIZE (1..max-length)!exceeds-max-length)
        }
        exceeds-max-length INTEGER ::= 999
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_02() {
      val text = """
        my-abstract-syntax {INTEGER:maxSize} ABSTRACT-SYNTAX ::= {
          my-PDU{size-max} IDENTIFIED BY {
            iso member-body(2) f(250) type-org(1) ft(16) asn1-book(9) chapter17(4) my-PDU(0)
          }
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
