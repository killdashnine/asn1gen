import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch13 {
  class TestS13S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
        PDV-List ::= SEQUENCE {
          transfer-syntax-name Transfer-syntax-name OPTIONAL,
          presentation-context-identifier Presentation-context-identifier,
          presentation-data-values CHOICE {
            single-ASN1-type [0] ABSTRACT-SYNTAX.&Type (
              CONSTRAINED BY {
                -- Type corresponding --
                -- to presentation-context-identifier --
              }
            ),
            octet-aligned [1] IMPLICIT OCTET STRING,
            arbitrary [2] IMPLICIT BIT STRING
          }
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
        Reject ::= SEQUENCE {
          invokeId InvokeId,
          problem CHOICE {
            general [0] GeneralProblem,
            invoke [1] InvokeProblem,
            returnResult [2] ReturnResultProblem,
            returnError [3] ReturnErrorProblem
          }
        } (
          CONSTRAINED BY {
            -- must conform to the above --
            -- definition--
          } ! RejectProblem:general-mistypedPDU
        )
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_3() {
      val text = """
        Encrypted{TypeToBeEnciphered} ::= BIT STRING (CONSTRAINED BY {-- must be the result of the encipherment --
-- of some BER-encoded value of --
TypeToBeEnciphered} !Error:securityViolation) Error ::= ENUMERATED {securityViolation}
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_4() {
      val text = """
        E ::= INTEGER (1..10, ...!Exception:too-large-integer)
        Exception ::= ENUMERATED {too-large-integer, ...}
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_5() {
      val text = """
        ImplementedUnivStr{UniversalString:Level} ::= UniversalString (
          FROM ((Level UNION BasicLatin)) !characterSet-problem)
        characterSet-problem INTEGER ::= 4
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_6() {
      val text = """
        T ::= INTEGER (0..10, ...!10)
        U ::= T (2..6, ...!6)
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_7() {
      val text = """
        ImplementedUnivStgLevel1{UniversalString:ImplementedSubset} ::=
          UniversalString (
            ImplementedUnivStr{{Level1}} INTERSECTION ImplementedSubset,
            ...!level1-problem)
        level1-problem INTEGER ::= 5
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
