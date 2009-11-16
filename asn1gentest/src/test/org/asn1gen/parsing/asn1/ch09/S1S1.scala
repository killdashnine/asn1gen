import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch09 {
  class TestS1S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """" +
      		TypeReference ::= CHOICE { integer INTEGER, boolean BOOLEAN }
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
      		value-reference TypeReference ::= integer:12
      		"""
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3() {
      val text = """
          Pair ::= SEQUENCE { x INTEGER, y INTEGER }
          Couple ::= SEQUENCE { x INTEGER, y INTEGER }
          pair Pair ::= { x 5, y 13 }
          couple Couple ::= pair
          Lighter-state ::= ENUMERATED {
            on(0), off(1),
            out-of-order(2)
          }
          Kettle-state ::= ENUMERATED {
            on(0), off(1),
            out-of-order(2)
          }
          lighter Lighter-state ::= on
          kettle Kettle-state ::= lighter
      		"""
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4() {
      val text = """
          PrimeNumbers INTEGER ::= { 2 | 3 | 5 | 7 | 11 | 13 }
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
