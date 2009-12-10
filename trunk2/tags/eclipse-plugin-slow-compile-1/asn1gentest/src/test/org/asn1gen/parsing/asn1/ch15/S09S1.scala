import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch15 {
  class TestS09S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
        MECHANISM-NAME ::= TYPE-IDENTIFIER
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
        Authentication-value ::= CHOICE {
          charstring [0] IMPLICIT GraphicString,
          bitstring [1] BIT STRING,
          external [2] EXTERNAL,
          other [3] IMPLICIT SEQUENCE {
            other-mechanism-name MECHANISM-NAME.&id({ObjectSet}),
            other-mechanism-value MECHANISM-NAME.&Type
            ({ObjectSet}{@.other-mechanism-name})
          }
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1() {
      val text = """
        CHOICE {
          charstring [0] IMPLICIT GraphicString,
          bitstring [1] BIT STRING,
          external [2] EXTERNAL,
          other [3] IMPLICIT SEQUENCE {
            other-mechanism-name MECHANISM-NAME.&id({ObjectSet}),
            other-mechanism-value MECHANISM-NAME.&Type
            ({ObjectSet}{@.other-mechanism-name})
          }
        }
      """
      parse(choiceType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1() {
      val text = """
        charstring [0] IMPLICIT GraphicString,
        bitstring [1] BIT STRING,
        external [2] EXTERNAL,
        other [3] IMPLICIT SEQUENCE {
          other-mechanism-name MECHANISM-NAME.&id({ObjectSet}),
          other-mechanism-value MECHANISM-NAME.&Type
          ({ObjectSet}{@.other-mechanism-name})
        }
      """
      parse(alternativeTypeLists, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1_1() {
      val text = """
        charstring [0] IMPLICIT GraphicString
      """
      parse(namedType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1_2() {
      val text = """
        bitstring [1] BIT STRING
      """
      parse(namedType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1_3() {
      val text = """
        external [2] EXTERNAL
      """
      parse(namedType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1_4() {
      val text = """
        other [3] IMPLICIT SEQUENCE {
          other-mechanism-name MECHANISM-NAME.&id({ObjectSet}),
          other-mechanism-value MECHANISM-NAME.&Type
          ({ObjectSet}{@.other-mechanism-name})
        }
      """
      parse(namedType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1_4_1() {
      val text = """
        SEQUENCE {
          other-mechanism-name MECHANISM-NAME.&id({ObjectSet}),
          other-mechanism-value MECHANISM-NAME.&Type
          ({ObjectSet}{@.other-mechanism-name})
        }
      """
      parse(sequenceType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1_4_1_1() {
      val text = """
        other-mechanism-name MECHANISM-NAME.&id({ObjectSet}),
        other-mechanism-value MECHANISM-NAME.&Type
        ({ObjectSet}{@.other-mechanism-name})
      """
      parse(componentTypeLists, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1_4_1_1_1() {
      val text = """
        other-mechanism-name MECHANISM-NAME.&id({ObjectSet})
      """
      parse(componentType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1_4_1_1_2() {
      val text = """
        other-mechanism-value MECHANISM-NAME.&Type
        ({ObjectSet}{@.other-mechanism-name})
      """
      parse(componentType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1_4_1_1_2_1() {
      val text = """
        ({ObjectSet}{@.other-mechanism-name})
      """
      parse(constraint, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1_4_1_1_2_1_1() {
      val text = """
        {ObjectSet}{@.other-mechanism-name}
      """
      parse(constraintSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1_4_1_1_2_1_1_1() {
      val text = """
        {ObjectSet}{@.other-mechanism-name}
      """
      parse(generalConstraint, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1_4_1_1_2_1_1_1_1() {
      val text = """
        {ObjectSet}{@.other-mechanism-name}
      """
      parse(tableConstraint, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1_4_1_1_2_1_1_1_1_1() {
      val text = """
        {ObjectSet}{@.other-mechanism-name}
      """
      parse(componentRelationConstraint, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3() {
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
    
    @Test def test_4() {
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
    
    @Test def test_5() {
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
    
    @Test def test_5_1() {
      val text = """
        type-id DefinedObjectClass.&id ({ObjectSet}),
        value [0] DefinedObjectClass.&Type ({ObjectSet}{@.type-id})
      """
      parse(componentTypeList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5_1_1_1() {
      val text = """
        type-id DefinedObjectClass.&id ({ObjectSet})
      """
      parse(componentType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5_1_1_2() {
      val text = """
        value [0] DefinedObjectClass.&Type ({ObjectSet}{@.type-id})
      """
      parse(componentType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5_1_1_2_1() {
      val text = """
        value [0] DefinedObjectClass.&Type ({ObjectSet}{@.type-id})
      """
      parse(namedType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5_1_1_2_1_1() {
      val text = """
        [0] DefinedObjectClass.&Type ({ObjectSet}{@.type-id})
      """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5_1_1_2_1_1_1() {
      val text = """
        ({ObjectSet}{@.type-id})
      """
      parse(constraint, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_6() {
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
  }
}
