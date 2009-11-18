import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch12 {
  class TestS6S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
        Afters ::= CHOICE {
          cheese [0] IA5String,
          dessert [1] IA5String
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
        mine Afters ::= dessert:"profiteroles"
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3() {
      val text = """
        Alternative ::= ENUMERATED {cheese, dessert}
        Afters ::= CHOICE { cheese [0] IA5String, dessert [1] IA5String }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4() {
      val text = """
        T ::= CHOICE {a [0] INTEGER, b [1] NULL }
        U ::= SET {x [0] REAL, y T,
        z CHOICE {c [1] BIT STRING, d [2] OCTET STRING }}
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5() {
      val text = """
        U ::= SET {
          x [0] REAL,
          y [1] T,
          z [2] CHOICE {
            c [1] BIT STRING,
            d [2] OCTET STRING
          }
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_6() {
      val text = """
        ROS ::= CHOICE {
          invoke [1] Invoke,
          returnResult [2] ReturnResult,
          returnError [3] ReturnError,
          reject [4] Reject
        }
      """
      parse(typeAssignment, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_7() {
      val text = """
        CHOICE {
          teletexString  TeletexString,
          printableString PrintableString,
          universalString UniversalString
        }
      """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
