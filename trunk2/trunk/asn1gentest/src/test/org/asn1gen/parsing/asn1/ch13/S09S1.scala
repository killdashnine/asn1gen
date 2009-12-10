import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch13 {
  class TestS09S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
        Quadruple ::= SEQUENCE {
          alpha ENUMERATED {state1, state2, state3},
          beta IA5String OPTIONAL,
          gamma SEQUENCE OF INTEGER,
          delta BOOLEAN DEFAULT TRUE
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_2() {
      val text = """
        Quadruple1 ::= Quadruple (WITH COMPONENTS { ..., alpha (state1), gamma (SIZE (5)) })
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_3() {
      val text = """
        Quadruple1 ::= SEQUENCE {
          alpha ENUMERATED {state1, state2, state3} (state1),
          beta IA5String OPTIONAL,
          gamma SEQUENCE SIZE (5) OF INTEGER,
          delta BOOLEAN DEFAULT TRUE
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_4() {
      val text = """
        Quadruple2 ::= Quadruple (WITH COMPONENTS {
          alpha (state1),
          beta (SIZE (5|12)) PRESENT,
          gamma (SIZE (5)),
          delta OPTIONAL
        })
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_5() {
      val text = """
        Quadruple3 ::= Quadruple (WITH COMPONENTS {alpha, beta, gamma})
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_6() {
      val text = """
        Quadruple3 ::= SEQUENCE {
          alpha ENUMERATED {state1, state2, state3},
          beta IA5String,
          gamma SEQUENCE OF INTEGER
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_7() {
      val text = """
        ConstrainedReal ::= REAL (
          WITH COMPONENTS {
            mantissa (-65535..65536),
            base (2),
            exponent (-127..128)
          }
        )
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_8() {
      val text = """
        ROIV-m-Linked-Reply-Action ::=
          ROIV-m-Linked-Reply (
            WITH COMPONENTS {
              invokedID PRESENT,
              linked-ID PRESENT,
              operation-value (m-Linked-Reply),
              argument (
                INCLUDES LinkedReplyArgument (
                  WITH COMPONENTS {
                    getResult ABSENT,
                    getListError ABSENT,
                    setResult ABSENT,
                    setListError ABSENT,
                    actionResult PRESENT,
                    processingFailure PRESENT,
                    deleteResult ABSENT,
                    actionError PRESENT,
                    deleteError ABSENT
                  }
                )
              )
            }
          )
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_10() {
      val text = """
        Choice ::= CHOICE { a A, b B, c C, d D}
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_11() {
      val text = """
        ChoiceCD ::= Choice (WITH COMPONENTS {..., a ABSENT, b ABSENT})
        ChoiceA1 ::= Choice (WITH COMPONENTS {..., a PRESENT})
        ChoiceA2 ::= Choice (WITH COMPONENTS {a PRESENT})
        ChoiceBCD ::= Choice (WITH COMPONENTS {a ABSENT, b, c})
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
