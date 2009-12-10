import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch13 {
  class TestS05S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
        Exactly31BitsString ::= BIT STRING (SIZE (31))
        StringOf31BitsAtTheMost ::= BIT STRING (SIZE (0..31))
        EvenNumber ::= INTEGER (2|4|6|8|10)
        EvenLengthString ::= IA5String (SIZE (INCLUDES EvenNumber))
        NonEmptyString ::= OCTET STRING (SIZE (1..MAX))
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
        ListOfStringsOf5Characters ::= SEQUENCE OF PrintableString (SIZE (5))
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3() {
      val text = """
        ListOfStrings ::= SEQUENCE OF PrintableString
        ListOf5Strings ::= ListOfStrings (SIZE (5))
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4() {
      val text = """
        ListOf5Strings ::= SEQUENCE (SIZE (5)) OF PrintableString
        ListOf5StringsOf5Characters ::=
          SEQUENCE (SIZE (5)) OF PrintableString (SIZE (5))
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5() {
      val text = """
        ListOf5StringsOf5Characters ::= SEQUENCE SIZE (5) OF PrintableString (SIZE (5))
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
