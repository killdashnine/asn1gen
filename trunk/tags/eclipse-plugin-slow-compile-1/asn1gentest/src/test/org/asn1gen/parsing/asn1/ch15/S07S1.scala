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
    
    @Test def test_1_1() {
      val text = """
        surname ATTRIBUTE ::= { -- family name
          SUBTYPE OF name
          WITH SYNTAX DirectoryString
          ID id-at-surname
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_2() {
      val text = """
        givenName ATTRIBUTE ::= { -- first name
          SUBTYPE OF name
          WITH SYNTAX DirectoryString
          ID id-at-givenName
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_3() {
      val text = """
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
    
    @Test def test_1_3_1() {
      val text = """
        { -- country
          SUBTYPE OF name
          WITH SYNTAX PrintableString (SIZE (2)) -- [ISO3166] codes
          SINGLE VALUE TRUE
          ID id-at-countryName
        }
      """
      parse(object_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_3_1_1() {
      val text = """
        { -- country
          SUBTYPE OF name
          WITH SYNTAX PrintableString (SIZE (2)) -- [ISO3166] codes
          SINGLE VALUE TRUE
          ID id-at-countryName
        }
      """
      parse(objectDefn, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_3_1_2x() {
      val text = """
        { -- country
          SUBTYPE OF name
          WITH SYNTAX PrintableString (SIZE (2)) -- [ISO3166] codes
          SINGLE VALUE TRUE
          ID id-at-countryName
        }
      """
      parse(definedObject, text) match {
        case Success(_, _) => fail("Parse failure expected")
        case x => ()
      }
    }
    
    @Test def test_1_3_1_3x() {
      val text = """
        { -- country
          SUBTYPE OF name
          WITH SYNTAX PrintableString (SIZE (2)) -- [ISO3166] codes
          SINGLE VALUE TRUE
          ID id-at-countryName
        }
      """
      parse(objectFromObject, text) match {
        case Success(_, _) => fail("Parse failure expected")
        case x => ()
      }
    }
    
    @Test def test_1_3_1_4x() {
      val text = """
        { -- country
          SUBTYPE OF name
          WITH SYNTAX PrintableString (SIZE (2)) -- [ISO3166] codes
          SINGLE VALUE TRUE
          ID id-at-countryName
        }
      """
      parse(parameterizedObject, text) match {
        case Success(_, _) => fail("Parse failure expected")
        case x => ()
      }
    }
    
    @Test def test_1_3_1_4_1x() {
      val text = """
        { -- country
          SUBTYPE OF name
          WITH SYNTAX PrintableString (SIZE (2)) -- [ISO3166] codes
          SINGLE VALUE TRUE
          ID id-at-countryName
        }
      """
      parse(defaultSyntax, text) match {
        case Success(_, _) => fail("Parse failure expected")
        case x => ()
      }
    }
    
    @Test def test_1_3_1_4_2() {
      val text = """
        { -- country
          SUBTYPE OF name
          WITH SYNTAX PrintableString (SIZE (2)) -- [ISO3166] codes
          SINGLE VALUE TRUE
          ID id-at-countryName
        }
      """
      parse(definedSyntax, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_3_1_4_2_1() {
      val text = """
        SUBTYPE OF name
        WITH SYNTAX PrintableString (SIZE (2)) -- [ISO3166] codes
        SINGLE VALUE TRUE
        ID id-at-countryName
      """
      parse(definedSyntaxToken.*, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_3_1_4_2_1_1() {
      val text = """
        SUBTYPE OF name
      """
      parse(definedSyntaxToken.*, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_3_1_4_2_1_2() {
      val text = """
        WITH SYNTAX PrintableString (SIZE (2)) -- [ISO3166] codes
      """
      parse(definedSyntaxToken.*, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_3_1_4_2_1_2_1() {
      val text = """
        WITH
      """
      parse(definedSyntaxToken, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_3_1_4_2_1_2_2() {
      val text = """
        SYNTAX
      """
      parse(definedSyntaxToken, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_3_1_4_2_1_2_3() {
      val text = """
        PrintableString (SIZE (2))
      """
      parse(definedSyntaxToken, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
        
    @Test def test_1_3_1_4_2_1_3() {
      val text = """
        SINGLE VALUE TRUE
      """
      parse(definedSyntaxToken.*, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_3_1_4_2_1_4() {
      val text = """
        ID id-at-countryName
      """
      parse(definedSyntaxToken.*, text) match {
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
  }
}
