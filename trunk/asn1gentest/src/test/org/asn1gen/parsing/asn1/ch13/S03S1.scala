import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch13 {
  class TestS03S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
        Number ::= INTEGER
        From3to15 ::= Number (3..15)
        From3excludedTo15excluded ::= Number (3<..<15)
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
        PositiveOrZeroNumber ::= Number (0..MAX)
        PositiveNumber ::= Number (0<..MAX)
        NegativeOrZeroNumber ::= Number (MIN..0)
        NegativeNumber ::= Number (MIN..<0)
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3() {
      val text = """
        T ::= REAL (0..<{mantissa 5,base 10,exponent 0})
        U ::= T ({mantissa 2,base 10,exponent 0}..MAX)
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4() {
      val text = """
        U ::= REAL ({mantissa 2,base 10,exponent 0}.. <{mantissa 5,base 10,exponent 0})
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5() {
      val text = """
        Interval ::= INTEGER {one(1), two(2)} (one..two)
        Enumeration ::= ENUMERATED {one(1), two(2)}
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
