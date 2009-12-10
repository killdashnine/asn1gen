import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch12 {
  class TestS8S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_01() {
      val text = """
        v ANY ::= INTEGER:12
        T ::= SEQUENCE {
          a BOOLEAN,
          b REAL
        }
        w ANY ::= T:{
          a TRUE,
          b {314, 10, -2}
        }
        """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    // Not supported
    def not_test_02() {
      val text = """
        Error ::= SEQUENCE {
          code INTEGER,
          parameter ANY DEFINED BY code
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    // Not supported
    def not_test_03() {
      val text = """
        ROIVapdu ::= SEQUENCE {
          invokeID InvokeIDType,
          linkedID [0] IMPLICIT InvokeIDType OPTIONAL,
          operation-value OPERATION,
          argument ANY DEFINED BY operation-value OPTIONAL
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    // Not supported
    def not_test_04() {
      val text = """
        AttributeValueAssertion ::= SEQUENCE {
          attribute OBJECT IDENTIFIER,
          is Operator,
          value [0] ANY DEFINED BY attribute
        }
        Operator ::= ENUMERATED {
          equalTo(0),
          greaterThan(1),
          greaterOrEqualTo(2),
          lessThan(3),
          lessOrEqualTo(4),
          notEqualTo(5)
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
