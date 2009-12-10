import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch12 {
  class TestS1S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
          T ::= [5] INTEGER Afters ::= CHOICE {
            cheese [0] IA5String,
            dessert [1] IA5String
          }
          
          ClientNumber ::= [APPLICATION 0] NumericString
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
          Afters ::= CHOICE { cheese IA5String, dessert IA5String }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3() {
      val text = """
          Afters ::= CHOICE { cheese [0] IA5String, dessert [1] IA5String }
          Form ::= SET {
            name Surname,
            first-name First-name,
            phone-number [2] NumericString
          }
          Surname ::= [0] VisibleString
          First-name ::= [1] VisibleString
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4() {
      val text = """
          A-possible-type ::= SET {
            integer [0] CHOICE {
              a [0] INTEGER,
              b [1] INTEGER
            },
            boolean [1] CHOICE {
              a [0] BOOLEAN,
              b [1] BOOLEAN
            }
          }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5() {
      val text = """
          Order-number ::= [APPLICATION 0] NumericString (SIZE (12))
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_6() {
      val text = """
          RejectTPDU ::= SET {
            destRef  [0]          Reference,
            yr-tu-nr [1]          TPDUnumber,
            credit   [2]          Credit,
            extended [PRIVATE 0]  BOOLEAN DEFAULT FALSE
          }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
