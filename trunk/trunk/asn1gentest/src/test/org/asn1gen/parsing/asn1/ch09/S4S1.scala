import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch09 {
  class TestS4S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
          T1 ::= [0] SET {
            name INTEGER,
            age PrintableString,
            male BOOLEAN
          }

          U1 ::= [1] SET {
            male Male,
            name VisibleString,
            age INTEGER
          }

          Male ::= BOOLEAN
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
          SET {
            age INTEGER,
            name UniversalString,
            male BOOLEAN
          }
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3() {
      val text = """
          T2 ::= [0] SEQUENCE {
            name [0] PrintableString,
            age INTEGER
          }
      
          U2 ::= [1] SEQUENCE {
            name [1] PrintableString,
            age INTEGER
          }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
