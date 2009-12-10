import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch13 {
  class TestS08S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
        TextBlock ::= SEQUENCE OF VisibleString
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_2() {
      val text = """
        AddressBlock ::= TextBlock (WITH COMPONENT (SIZE (1..32)))
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_3() {
      val text = """
        DigitBlock ::= TextBlock (WITH COMPONENT (NumericString))
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_4() {
      val text = """
        AddressBlock ::= SEQUENCE OF VisibleString (SIZE (1..32))
        DigitBlock ::= SEQUENCE OF VisibleString (NumericString)
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_5() {
      val text = """
        IntegerMatrix ::= SEQUENCE SIZE (6) OF SEQUENCE SIZE (6) OF INTEGER
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_6() {
      val text = """
        CoordinateMatrix ::= IntegerMatrix (WITH COMPONENT (WITH COMPONENT (-100..100)))
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_7() {
      val text = """
        CoordinateMatrix ::= SEQUENCE SIZE (6) OF SEQUENCE SIZE (6) OF INTEGER (-100..100)
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
