import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch15 {
  class TestS08S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
        AttributeIdAndValue3 ::= SEQUENCE {
          ident ATTRIBUTE.&id({SupportedAttributes}),
          value ATTRIBUTE.&Type({SupportedAttributes}{@.ident})
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
        AttributeIdsAndValues ::= SET OF SEQUENCE {
          ident ATTRIBUTE.&id({SupportedAttributes}),
          value ATTRIBUTE.&Type({SupportedAttributes}{@.ident})
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3() {
      val text = """
        AttributeValueAssertion ::= SEQUENCE {
          type ATTRIBUTE.&Id({SupportedAttributes}),
          assertion ATTRIBUTE.&equality-match.&AssertionType({SupportedAttributes}{@type})
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4() {
      val text = """
        FilterItem ::= CHOICE {
          equality  [0] AttributeValueAssertion,
          substrings [1] SEQUENCE {
            type  Attribute.&id({SupportedAttributes}),
            strings SEQUENCE OF CHOICE {
              initial [0] ATTRIBUTE.&Type ({SupportedAttributes}{@substrings.type}),
              any [1] ATTRIBUTE.&Type ({SupportedAttributes}{@substrings.type}),
              final [2] ATTRIBUTE.&Type ({SupportedAttributes}{@substrings.type})
            }
          },
          greaterOrEqual [2] AttributeValueAssertion,
          lessOrEqual [3] AttributeValueAssertion,
          present [4] AttributeType,
          approximateMatch [5] AttributeValueAssertion,
          extensibleMatch [6] MatchingRuleAssertion
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5() {
      val text = """
        Attribute-desc ::= SEQUENCE {
          usage ATTRIBUTE.&usage({SupportedAttributes}),
          list SEQUENCE OF SEQUENCE {
            ident ATTRIBUTE.&id({SupportedAttributes}{@usage}),
            value ATTRIBUTE.&Type ({SupportedAttributes}{@usage,@.ident})
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
        att-desc Attribute-desc ::= {
          usage userApplications,
          list {
            { ident id-at-objectClass, value oid },
            { ident id-at-aliasedEntryName, value distinguishedName }
          }
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
