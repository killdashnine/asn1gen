import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch12 {
  class TestS2S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_01() {
      val text = """
          Description ::= SEQUENCE {
            surname  IA5String,
            first-name IA5String,
            age INTEGER
          }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_02() {
      val text = """
          johnny Description ::= {
            surname  "Smith",
            first-name "John",
            age 40
          }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_03() {
      val text = """
          Message ::= SEQUENCE {
            envelope MessageTransferEnvelope,
            content Content
          }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_04() {
      val text = """
          ImprovedDescription ::= SEQUENCE {
            surname IA5String,
            first-name IA5String OPTIONAL,
            age INTEGER DEFAULT 40
          }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_05() {
      val text = """
          T ::= SEQUENCE {
            x INTEGER,
            y INTEGER OPTIONAL,
            z INTEGER DEFAULT 0,
            t INTEGER
          }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_06() {
      val text = """
          T ::= SEQUENCE {
            x INTEGER, -- [UNIVERSAL 2]
            y [0] INTEGER OPTIONAL,
            z [1] INTEGER DEFAULT 0,
            t INTEGER -- [UNIVERSAL 2]
          }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_07() {
      val text = """
          Registration ::= SEQUENCE {
            COMPONENTS OF Description,
            marital-status ENUMERATED {
              single, married, divorced, widowed
            }
          }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_08() {
      val text = """
        Registration ::= SEQUENCE {
          surname IA5String,
          first-name IA5String,
          age INTEGER,
          marital-status ENUMERATED {single, married, divorced, widowed}
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_09() {
      val text = """
        OtherRegistration ::= SEQUENCE {
          description Description,
          marital-status ENUMERATED {
            single, married, divorced, widowed
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
