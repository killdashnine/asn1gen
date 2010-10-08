import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch13 {
  class TestS02S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
          Two ::= INTEGER (2)
          Day ::= ENUMERATED {
            monday(0), tuesday(1), wednesday(2), thursday(3),
            friday(4), saturday(5), sunday(6)
          }
          Wednesday ::= Day (wednesday)
          FourZ ::= IA5String ("ZZZZ")
          Afters ::= CHOICE {
            cheese IA5String,
            dessert ENUMERATED {
              profiterolles(1), sabayon(2), fraisier(3)
            }
          }
          CompulsoryAfters ::= Afters (dessert:sabayon)
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
          WeekEnd ::= Day (saturday|sunday)
          PushButtonDial ::= IA5String ("0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"|"*"|"#")
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
