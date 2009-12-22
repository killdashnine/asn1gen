import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch17 {
  class TestS2S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_01() {
      val text = """
        DirectoryString{INTEGER:maxSize} ::= CHOICE {
          teletexString TeletexString (SIZE (1..maxSize)),
          printableString PrintableString (SIZE (1..maxSize)),
          universalString UniversalString (SIZE (1..maxSize)),
          bmpString BMPString (SIZE (1..maxSize)),
          utf8String UTF8String (SIZE (1..maxSize))
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_02() {
      val text = """
        SubstringAssertion ::= SEQUENCE OF CHOICE {
          initial [0] DirectoryString{ub-match},
          any [1] DirectoryString{ub-match},
          final  [2] DirectoryString{ub-match}
        }
        ub-match INTEGER ::= 128
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_03() {
      val text = """
        CHOICE {
          teletexString TeletexString (SIZE (1..ub-match)),
          printableString PrintableString (SIZE (1..ub-match)),
          universalString UniversalString (SIZE (1..ub-match)),
          bmpString BMPString (SIZE (1..ub-match)),
          utf8String UTF8String (SIZE (1..ub-match))
        }
      """
      parse(_type, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_04() {
      val text = """
        SubstringAssertion{INTEGER:ub-match} ::= SEQUENCE OF CHOICE {
          initial [0] DirectoryString{ub-match},
          any  [1] DirectoryString{ub-match},
          final [2] DirectoryString{ub-match}
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_05() {
      val text = """
        T ::= INTEGER
        List{T} ::= SEQUENCE OF T
        """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_06() {
      val text = """
        Choice{T} ::= CHOICE {
          a [0] T,
          b INTEGER
        }
        Structure{T} ::= SEQUENCE {
          a INTEGER,
          b [0] T OPTIONAL,
          c INTEGER
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_07() {
      val text = """
        GeneralForm{T, T:val} ::= SEQUENCE {
          info T DEFAULT val,
          comments IA5String
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_08() {
      val text = """
        Form ::= GeneralForm{BOOLEAN, TRUE}
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_09() {
      val text = """
        Form ::= SEQUENCE {
          info  BOOLEAN DEFAULT TRUE,
          comments IA5String
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_10() {
      val text = """
        pariTierce {INTEGER:first, INTEGER:second, INTEGER:third} SEQUENCE OF INTEGER ::= {
          first, second, third
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_11() {
      val text = """
        MESSAGE-PARAMETERS ::= CLASS {
          &max-priority-level INTEGER,
          &max-message-buffer-size INTEGER,
          &max-reference-buffer-size INTEGER
        } WITH SYNTAX {
          MAXIMUM PRIORITY &max-priority-level
          MAXIMUM MESSAGE BUFFER &max-message-buffer-size
          MAXIMUM REFERENCE BUFFER &max-reference-buffer-size
        }
        Message-PDU{MESSAGE-PARAMETERS:param} ::= SEQUENCE {
          priority INTEGER (0..param.&max-priority-level!Exception:priority),
          message UTF8String (SIZE(0..param.&max-message-buffer-size)!Exception:message),
          comments UTF8String (SIZE(0..param.&max-reference-buffer-size)!Exception:comments)
        }
        Exception ::= ENUMERATED {
          priority(0), message(1), comments(2), ...
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_11_1() {
      val text = """
        MESSAGE-PARAMETERS ::= CLASS {
          &max-priority-level INTEGER,
          &max-message-buffer-size INTEGER,
          &max-reference-buffer-size INTEGER
        } WITH SYNTAX {
          MAXIMUM PRIORITY &max-priority-level
          MAXIMUM MESSAGE BUFFER &max-message-buffer-size
          MAXIMUM REFERENCE BUFFER &max-reference-buffer-size
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_11_1_1() {
      val text = """
        CLASS {
          &max-priority-level INTEGER,
          &max-message-buffer-size INTEGER,
          &max-reference-buffer-size INTEGER
        } WITH SYNTAX {
          MAXIMUM PRIORITY &max-priority-level
          MAXIMUM MESSAGE BUFFER &max-message-buffer-size
          MAXIMUM REFERENCE BUFFER &max-reference-buffer-size
        }
      """
      parse(objectClass, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_11_1_1_1x() {
      val text = """
        CLASS {
          &max-priority-level INTEGER,
          &max-message-buffer-size INTEGER,
          &max-reference-buffer-size INTEGER
        } WITH SYNTAX {
          MAXIMUM PRIORITY &max-priority-level
          MAXIMUM MESSAGE BUFFER &max-message-buffer-size
          MAXIMUM REFERENCE BUFFER &max-reference-buffer-size
        }
      """
      parse(parameterizedObjectClass, text) match {
        case Success(_, _) => fail("Parse failure expected")
        case x => ()
      }
    }
    
    @Test def test_11_1_1_2x() {
      val text = """
        CLASS {
          &max-priority-level INTEGER,
          &max-message-buffer-size INTEGER,
          &max-reference-buffer-size INTEGER
        } WITH SYNTAX {
          MAXIMUM PRIORITY &max-priority-level
          MAXIMUM MESSAGE BUFFER &max-message-buffer-size
          MAXIMUM REFERENCE BUFFER &max-reference-buffer-size
        }
      """
      parse(definedObjectClass, text) match {
        case Success(_, _) => fail("Parse failure expected")
        case x => ()
      }
    }
    
    @Test def test_11_1_1_3() {
      val text = """
        CLASS {
          &max-priority-level INTEGER,
          &max-message-buffer-size INTEGER,
          &max-reference-buffer-size INTEGER
        } WITH SYNTAX {
          MAXIMUM PRIORITY &max-priority-level
          MAXIMUM MESSAGE BUFFER &max-message-buffer-size
          MAXIMUM REFERENCE BUFFER &max-reference-buffer-size
        }
      """
      parse(objectClassDefn, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_11_1_1_3_1() {
      val text = """
        CLASS {
          &max-priority-level INTEGER,
          &max-message-buffer-size INTEGER,
          &max-reference-buffer-size INTEGER
        } WITH SYNTAX {
          MAXIMUM PRIORITY &max-priority-level
          MAXIMUM MESSAGE BUFFER &max-message-buffer-size
          MAXIMUM REFERENCE BUFFER &max-reference-buffer-size
        }
      """
      parse(kw("CLASS")
        ~ op("{")
        ~ rep1sep(fieldSpec, op(","))
        ~ op("}")
        ~ withSyntaxSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_11_1_1_3_1_1() {
      val text = """
        CLASS {
          &max-priority-level INTEGER,
          &max-message-buffer-size INTEGER,
          &max-reference-buffer-size INTEGER
        }
      """
      parse(kw("CLASS")
        ~ op("{")
        ~ rep1sep(fieldSpec, op(","))
        ~ op("}"), text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_11_1_1_3_1_2() {
      val text = """
        WITH SYNTAX {
          MAXIMUM PRIORITY &max-priority-level
          MAXIMUM MESSAGE BUFFER &max-message-buffer-size
          MAXIMUM REFERENCE BUFFER &max-reference-buffer-size
        }
      """
      parse(withSyntaxSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_11_1_1_3_1_2_1() {
      val text = """
        {
          MAXIMUM PRIORITY &max-priority-level
          MAXIMUM MESSAGE BUFFER &max-message-buffer-size
          MAXIMUM REFERENCE BUFFER &max-reference-buffer-size
        }
      """
      parse(syntaxList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_11_1_1_3_1_2_1_1() {
      val text = """
        MAXIMUM PRIORITY &max-priority-level
        MAXIMUM MESSAGE BUFFER &max-message-buffer-size
        MAXIMUM REFERENCE BUFFER &max-reference-buffer-size
      """
      parse(tokenOrGroupSpec.+, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_11_1_1_3_1_2_1_1_1() {
      val text = """
        MAXIMUM PRIORITY &max-priority-level
        MAXIMUM MESSAGE BUFFER &max-message-buffer-size
        MAXIMUM REFERENCE BUFFER &max-reference-buffer-size
      """
      parse(tokenOrGroupSpec.+, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_11_2() {
      val text = """
        Message-PDU{MESSAGE-PARAMETERS:param} ::= SEQUENCE {
          priority INTEGER (0..param.&max-priority-level!Exception:priority),
          message UTF8String (SIZE(0..param.&max-message-buffer-size)!Exception:message),
          comments UTF8String (SIZE(0..param.&max-reference-buffer-size)!Exception:comments)
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_11_3() {
      val text = """
        Exception ::= ENUMERATED {
          priority(0), message(1), comments(2), ...
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_12() {
      val text = """
        Forward{OPERATION:OperationSet} OPERATION ::= {
          OperationSet |
          OperationSet.&Linked.&Linked |
          OperationSet.&Linked.&Linked.&Linked.&Linked
        }
        Reverse{OPERATION:OperationSet} OPERATION ::= {
          Forward{{OperationSet.&Linked}}
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_13() {
      val text = """
        ModuleName DEFINITIONS ::=
          BEGIN
            EXPORTS Forward{}, Reverse{}, ForwardAndReverse;
            IMPORTS
              OPERATION FROM Remote-Operations-Information-Objects {
                joint-iso-itu-t remote-operations(4)
                informationObjects(5) version1(0)}
              Forward{}, Reverse{} FROM
                Remote-Operations-Useful-Definitions {
                  joint-iso-itu-t remote-operations(4) useful-definitions(7) version1(0)
                };
            -- dynamically extensible object set:
            MyOperationSet OPERATION ::= {...}
            -- non-parameterized definition:
            ForwardAndReverse OPERATION ::= {
              Forward{{MyOperationSet}} UNION Reverse{{MyOperationSet}}
            }
          END
      """
      parse(moduleDefinition, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
