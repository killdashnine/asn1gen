import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch10 {
  class TestS6S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_01() {
      val text = """
          BitString ::= BIT STRING
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_02() {
      val text = """
          pi-decimals BIT STRING ::=
            '00100100001111110110101010001000100001
            01101000110000100011010011000100110001
            100110001010001011100000001101110000'B
          pi-decimals BIT STRING ::=
            '243F6A8885A308D313198A2E0370'H
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_03() {
      val text = """
          StringOf32Bits ::= BIT STRING (SIZE (32))
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_04() {
      val text = """
          Rights ::= BIT STRING {
            user-read(0), user-write(1), group-read(2), group-write(3),
            other-read(4), other-write(5)
          }
          group1 Rights ::= {
            group-read, group-write
          }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_05() {
      val text = """
          group2 Rights ::= '0011'B
          group2 Rights ::= '3'H
          group3 Rights ::= '001100'B
          weird-rights Rights ::= '0000001'B
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_06() {
      val text = """
          alpha INTEGER ::= 1
          BinaryString ::= BIT STRING { alpha(3), beta(alpha) }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_07() {
      val text = """
          Versions ::= BIT STRING { version-1(0), version-2(1) }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_08() {
      val text = """
          BooleanSequence ::= SEQUENCE {
            b1 BOOLEAN,
            b2 BOOLEAN,
            b3 BOOLEAN,
            b4 BOOLEAN,
            b5 BOOLEAN,
            b6 BOOLEAN
          }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_09() {
      val text = """
          BooleanVector ::= BIT STRING { b1(0), b2(1), b3(2), b4(3), b5(4), b6(5) }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_10() {
      val text = """
          all-wrong BooleanVector (SIZE (6)) ::= {}
          """
      parse(valueAssignment, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
