import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch15 {
  class TestS04S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
        ATTRIBUTE ::= CLASS {
          &derivation ATTRIBUTE OPTIONAL,
          &Type OPTIONAL,
          &equality-match MATCHING-RULE OPTIONAL,
          &ordering-match MATCHING-RULE OPTIONAL,
          &substrings-match MATCHING-RULE OPTIONAL,
          &single-valued BOOLEAN DEFAULT FALSE,
          &collective BOOLEAN DEFAULT FALSE,
          &no-user-modification BOOLEAN DEFAULT FALSE,
          &usage Attribute-Usage DEFAULT userApplications,
          &id OBJECT IDENTIFIER UNIQUE
        } WITH SYNTAX {
          [SUBTYPE OF &derivation]
          [WITH SYNTAX &Type]
          [EQUALITY MATCHING RULE &equality-match]
          [ORDERING MATCHING RULE &ordering-match]
          [SUBSTRINGS MATCHING RULE &substrings-match]
          [SINGLE VALUE &single-valued]
          [COLLECTIVE &collective]
          [NO USER MODIFICATION &no-user-modification]
          [USAGE &usage]
          ID &id
        }
        AttributeUsage ::= ENUMERATED {
          userApplications(0),
          directoryOperation(1),
          distributedOperation(2),
          dSAOperation(3)
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
        MATCHING-RULE ::= CLASS {
          &AssertionType OPTIONAL,
          &id OBJECT IDENTIFIER UNIQUE
        } WITH SYNTAX {
          [SYNTAX ID &AssertionType] &id
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3() {
      val text = """
        name ATTRIBUTE ::= {
          WITH SYNTAX DirectoryString
          EQUALITY MATCHING RULE caseIgnoreMatch
          ID { joint-iso-itu-t ds(5) attributeType(4) 2}
        }
        DirectoryString ::= CHOICE {
          teletexString TeletexString (SIZE (1..maxSize)),
          printableString PrintableString (SIZE (1..maxSize)),
          universalString UniversalString (SIZE (1..maxSize)),
          bmpString BMPString (SIZE (1..maxSize)),
          utf8String UTF8String (SIZE (1..maxSize))
        }
        maxSize INTEGER ::= 25
        caseIgnoreMatch MATCHING-RULE ::= {
          SYNTAX DirectoryString ID {id-mr 2}
        }
        id-mr OBJECT IDENTIFIER ::= {
          joint-iso-itu-t ds(5) matchingRule(13)
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
