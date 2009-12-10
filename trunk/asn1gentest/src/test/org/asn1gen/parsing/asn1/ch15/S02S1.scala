import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch15 {
  class TestS02S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
        FUNCTION ::= CLASS {
          &ArgumentType,
          &ResultType DEFAULT NULL,
          &Errors ERROR OPTIONAL,
          &code INTEGER UNIQUE
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_1() {
      val text = """
        FUNCTION ::= CLASS {
          &ArgumentType,
          &ResultType DEFAULT NULL,
          &Errors ERROR OPTIONAL,
          &code INTEGER UNIQUE
        }
      """
      parse(objectClassAssignment, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_1_1_1_1() {
      val text = """
        &ArgumentType
      """
      parse(fieldSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_1_1_1_2() {
      val text = """
        &ResultType DEFAULT NULL
      """
      parse(fieldSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_1_1_1_3() {
      val text = """
        &Errors ERROR OPTIONAL
      """
      parse(fieldSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_1_1_1_4() {
      val text = """
        &code INTEGER UNIQUE
      """
      parse(fieldSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
