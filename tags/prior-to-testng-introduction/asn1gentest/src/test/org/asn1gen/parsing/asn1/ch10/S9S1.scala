import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch10 {
  class TestS9S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
          SEQUENCE {
            reference-node OBJECT IDENTIFIER DEFAULT {
              iso
              member-body(2) f(250) type-org(1)
              ft(16) asn1-book(9)
            },
            relative-oids SEQUENCE OF RELATIVE-OID
                          -- relative to reference-node --
          }
          """
      parse(_type, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
          CHOICE {
            absolute-oid OBJECT IDENTIFIER,
            relative-oids RELATIVE-OID
          }
          """
      parse(_type, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
