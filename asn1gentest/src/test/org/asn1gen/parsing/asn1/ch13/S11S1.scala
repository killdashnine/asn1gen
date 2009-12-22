import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch13 {
  class TestS11S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
        PhoneNumber ::= NumericString (FROM ("0".."9"))(SIZE (10))
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_2() {
      val text = """
        Row ::= SEQUENCE OF INTEGER
        CoordinateMatrix ::= SEQUENCE SIZE (6) OF Row (SIZE (6))(WITH COMPONENT (-100..100))
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_3() {
      val text = """
        TextBlock ::= SEQUENCE OF VisibleString Address ::=
          TextBlock (SIZE (3..6))(WITH COMPONENT (SIZE (1..32)))
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_4() {
      val text = """
        PushButtonDialSequence ::= IA5String (FROM ("0".."9"|"*"|"#"))
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_5() {
      val text = """
        Lipogramme ::= IA5String (FROM (ALL EXCEPT ("e"|"E")))
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_6() {
      val text = """
        SaudiName ::= BasicArabic (SIZE (1..100) ^ Level2)
        ISO-10646-String ::= BMPString (
          FROM (Level2 ^ (BasicLatin | HebrewExtended | Hiragana))
        )
        KatakanaAndBasicLatin ::= UniversalString (FROM (Katakana | BasicLatin))
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_7() {
      val text = """
        CapitalAndSmall ::= IA5String (FROM ("A".."Z"|"a".."z"))
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_8() {
      val text = """
        CapitalOrSmall ::= IA5String (FROM ("A".."Z")|FROM ("a".."z"))
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_10() {
      val text = """
        ExoticString ::= IA5String (SIZE (1..4)|FROM ("abc"))
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_11() {
      val text = """
        InvokeId ::= CHOICE { present INTEGER, absent NULL }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_12() {
      val text = """
        DAP-InvokeIdSet ::= InvokeId (ALL EXCEPT absent:NULL)
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_13() {
      val text = """
        Identifications ::= SEQUENCE {
          idNumber NumericString (FROM (ALL EXCEPT " "))(SIZE (6)) OPTIONAL,
          telephone NumericString (FROM (ALL EXCEPT " "))(SIZE (13)) OPTIONAL
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_14() {
      val text = """
        Person ::= SEQUENCE {
          name PrintableString (SIZE (1..20)),
          ident Identifications (
            WITH COMPONENTS {idNumber} | WITH COMPONENTS {telephone}
          )
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
