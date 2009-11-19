import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch12 {
  class TestS1S3 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
          ModuleName DEFINITIONS IMPLICIT TAGS ::=
            BEGIN
              I ::= INTEGER
            END
          """
      parse(moduleDefinition, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
          M1 DEFINITIONS EXPLICIT TAGS ::=
            BEGIN
              T1 ::= CHOICE {
                cheese [0] IA5String,
                dessert [1] IA5String
              }
            END
          """
      parse(moduleDefinition, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3() {
      val text = """
          M2 DEFINITIONS IMPLICIT TAGS ::=
            BEGIN
              IMPORTS T1 FROM M1;
      
              T2 ::= SET {
                a [0] T1,
                b [1] REAL
              }
            END
          """
      parse(moduleDefinition, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4() {
      val text = """
          M1 DEFINITIONS ::=
            BEGIN
              T1 ::= CHOICE {
                cheese [0] EXPLICIT IA5String,
                dessert [1] EXPLICIT IA5String
              }
            END
          """
      parse(moduleDefinition, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5() {
      val text = """
          M2 DEFINITIONS ::=
            BEGIN
              T1 ::= CHOICE {
                cheese [0] EXPLICIT IA5String,
                dessert [1] EXPLICIT IA5String
              }
              T2 ::= SET {
                a [0] EXPLICIT T1,
                b [1] IMPLICIT REAL
              }
            END
          """
      parse(moduleDefinition, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_6() {
      val text = """
          M DEFINITIONS AUTOMATIC TAGS ::=
            BEGIN
              T ::= SEQUENCE {
                a INTEGER,
                b CHOICE {
                  i INTEGER,
                  n NULL
                },
                c REAL
              }
            END
          """
      parse(moduleDefinition, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_6_1() {
      val text = """
        T ::= SEQUENCE {
          a INTEGER,
          b CHOICE {
            i INTEGER,
            n NULL
          },
          c REAL
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_6_1_1() {
      val text = """
        SEQUENCE {
          a INTEGER,
          b CHOICE {
            i INTEGER,
            n NULL
          },
          c REAL
        }
      """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_6_1_1_1() {
      val text = """
        SEQUENCE {
          a INTEGER,
          b CHOICE {
            i INTEGER,
            n NULL
          },
          c REAL
        }
      """
      parse(sequenceType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_6_1_1_1_1() {
      val text = """
        a INTEGER,
        b CHOICE {
          i INTEGER,
          n NULL
        },
        c REAL
      """
      parse(componentTypeLists, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_7() {
      val text = """
          M DEFINITIONS ::=
            BEGIN
              T ::= SEQUENCE {
                a [0] IMPLICIT INTEGER,
                b [1] EXPLICIT CHOICE {
                  i [0] IMPLICIT INTEGER,
                  n [1] IMPLICIT NULL
                },
                c [2] IMPLICIT REAL
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
