import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch15 {
  class TestS07S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
        surname ATTRIBUTE ::= { -- family name
          SUBTYPE OF name
          WITH SYNTAX DirectoryString
          ID id-at-surname
        }
        givenName ATTRIBUTE ::= { -- first name
          SUBTYPE OF name
          WITH SYNTAX DirectoryString
          ID id-at-givenName
        }
        countryName ATTRIBUTE ::= { -- country
          SUBTYPE OF name
          WITH SYNTAX PrintableString (SIZE (2)) -- [ISO3166] codes
          SINGLE VALUE TRUE
          ID id-at-countryName
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
        SupportedAttributes ATTRIBUTE ::= {surname | givenName | countryName}
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3() {
      val text = """
        AttributeIdAndValue1 ::= SEQUENCE {
          ident ATTRIBUTE.&id,
          value ATTRIBUTE.&Type
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4() {
      val text = """
        AttributeIdAndValue2 ::= SEQUENCE {
          ident ATTRIBUTE.&id({SupportedAttributes}),
          value ATTRIBUTE.&Type({SupportedAttributes})
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5() {
      val text = """
        AttributeIdAndValue2 ::= SEQUENCE {
          ident SupportedAttributes.&id,
          value SupportedAttributes.&Type
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_6() {
      val text = """
        value AttributeIdAndValue2 ::= {
          ident id-at-countryName,
          value DirectoryString:universalString:"$$Escher$$"
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_7() {
      val text = """
        AttributeIdAndValue3 ::= SEQUENCE {
          ident ATTRIBUTE.&id({SupportedAttributes}),
          value ATTRIBUTE.&Type({SupportedAttributes}{@ident})
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_8() {
      val text = """
        AttributeIdAndValue3 ::= SEQUENCE {
          ident OBJECT IDENTIFIER,
          value ANY DEFINED BY ident
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}