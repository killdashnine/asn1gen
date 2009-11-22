import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch15 {
  class TestS06S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
        caseIgnoreMatchValue caseIgnoreMatch.&AssertionType ::= printableString:"Escher"
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
        id-mr-caseIgnoreMatch OBJECT IDENTIFIER ::= caseIgnoreMatch.&id
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3() {
      val text = """
        CLASS1 ::= CLASS { &obj CLASS2 }
        CLASS2 ::= CLASS { &val INTEGER }
        object1 CLASS1 ::= { &obj object2 }
        object2 CLASS2 ::= { &val 5 }
        value INTEGER ::= object1.&obj.&val
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4() {
      val text = """
        Oids OBJECT IDENTIFIER ::= {MatchingRules.&id}
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5() {
      val text = """
        Oids OBJECT IDENTIFIER ::= { {id-mr 2} | {id-mr 12} | {id-mr 13} }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_6() {
      val text = """
        SupportedFunctions OTHER-FUNCTION ::= {
          addition-of-2-integers | substraction-of-2-integers | multiplication-of-2-integers
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
