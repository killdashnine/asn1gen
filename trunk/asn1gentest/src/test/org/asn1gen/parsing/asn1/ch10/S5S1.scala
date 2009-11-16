import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch10 {
  class TestS5S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """" +
          ExtendedReal ::= CHOICE {
            decimal REAL,
            particular-real ENUMERATED {
              one-third,
              pi,
              e,
              ...
            }
          }
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
          pi REAL ::= { mantissa 314159, base 10, exponent -5 }
          e REAL ::= { mantissa 271828128459045235360287, base 10, exponent -23 }
          zero REAL ::= 0
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3() {
      val text = """
          SEQUENCE {
            mantissa INTEGER (ALL EXCEPT 0),
            base INTEGER (2|10),
            exponent INTEGER
          }
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4() {
      val text = """
          pi REAL ::= { 314159, 10, -5 }
          e REAL ::= { 271828128459045235360287, 10, -23 }
          zero REAL ::= 0
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5() {
      val text = """" +
          BinaryReal ::= REAL (WITH COMPONENTS {..., base (2)})
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_6() {
      val text = """
          RestrictedReal ::= REAL (
            WITH COMPONENTS {
              mantissa (-16777215..16777215),
              base (2),
              exponent (-125..128)
            })
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_7() {
      val text = """
          
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_8() {
      val text = """
          
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
