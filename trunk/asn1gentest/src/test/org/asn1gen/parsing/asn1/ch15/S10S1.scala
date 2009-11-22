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
        ABSTRACT-SYNTAX ::= CLASS {
          &id OBJECT IDENTIFIER,
          &Type,
          &property BIT STRING {handles-invalid-encodings(0)} DEFAULT {}
        } WITH SYNTAX {
          &Type IDENTIFIED BY &id [HAS PROPERTY &property]
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
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
    
    @Test def test_3() {
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
    
    @Test def test_4() {
      val text = """
        SEQUENCE {
          type-id TYPE-IDENTIFIER.&id,
          value [0] EXPLICIT TYPE-IDENTIFIER.&Type
        }
      """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5() {
      val text = """
        SEQUENCE {
          type-id DefinedObjectClass.&id,
          value [0] EXPLICIT DefinedObjectClass.&Type
        }
      """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_6() {
      val text = """
        SEQUENCE {
          type-id DefinedObjectClass.&id ({ObjectSet}),
          value [0] DefinedObjectClass.&Type ({ObjectSet}{@.type-id})
        }
      """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_7() {
      val text = """
        ExtendedBodyPart ::= SEQUENCE {
          parameters [0] INSTANCE OF TYPE-IDENTIFIER OPTIONAL,
          data INSTANCE OF TYPE-IDENTIFIER
        } (CONSTRAINED BY {-- must correspond to the &parameters --
          -- and &data fields of a member of -- IPMBodyPartTable}
        )
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
