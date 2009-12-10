import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch15 {
  class TestS10S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
        ProtocolName-Abstract-Syntax-Module {
          iso member-body(2) f(250) type-org(1) ft(16) asn1-book(9) chapter15(3) protocol-name(0)
        } DEFINITIONS ::=
          BEGIN
            IMPORTS ProtocolName-PDU FROM ProtocolName-Module {
              iso member-body(2) f(250) type-org(1) ft(16)
              asn1-book(9) chapter15(3) protocol-name(0) module1(2)
            };

            protocolName-Abstract-Syntax ABSTRACT-SYNTAX ::= {
              ProtocolName-PDU IDENTIFIED BY protocolName-Abstract-Syntax-id
            }
            protocolName-Abstract-Syntax-id OBJECT IDENTIFIER ::= {
              iso member-body(2) f(250) type-org(1) ft(16) asn1-book(9)
              chapter15(3) protocol-name(0) abstract-syntax(0)
            }
            protocolName-Abstract-Syntax-descriptor ObjectDescriptor ::=
              "Abstract syntax of ProtocolName"
            protocolName-Transfer-Syntax-id OBJECT IDENTIFIER ::= {
              iso member-body(2) f(250) type-org(1) ft(16)
              asn1-book(9) chapter15(3) protocol-name(0) transfer-syntax(1)
            }
            protocolName-Transfer-Syntax-descriptor ObjectDescriptor ::=
              "Transfer syntax of ProtocolName"
          END
      """
      parse(moduleDefinition, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
        PDV-list ::= SEQUENCE {
          transfer-syntax-name Transfer-syntax-name OPTIONAL,
          presentation-context-identifier Presentation-context-identifier,
          presentation-data-values CHOICE {
            single-ASN1-type [0] ABSTRACT-SYNTAX.&Type (
              CONSTRAINED BY { -- Type which corresponds to --
                -- the presentation context identifier --
              }
            ),
            octet-aligned [1] IMPLICIT OCTET STRING,
            arbitrary  [2] IMPLICIT BIT STRING
          }
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
