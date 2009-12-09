import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch09 {
  class TestS1S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
      		TypeReference ::= CHOICE { integer INTEGER, boolean BOOLEAN }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
      		value-reference TypeReference ::= integer:12
      		"""
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3() {
      val text = """
          Pair ::= SEQUENCE { x INTEGER, y INTEGER }
          Couple ::= SEQUENCE { x INTEGER, y INTEGER }
          pair Pair ::= { x 5, y 13 }
          couple Couple ::= pair
          Lighter-state ::= ENUMERATED {
            on(0), off(1),
            out-of-order(2)
          }
          Kettle-state ::= ENUMERATED {
            on(0), off(1),
            out-of-order(2)
          }
          lighter Lighter-state ::= on
          kettle Kettle-state ::= lighter
      		"""
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_1() {
      val text = """
          Pair ::= SEQUENCE { x INTEGER, y INTEGER }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_2() {
      val text = """
          Couple ::= SEQUENCE { x INTEGER, y INTEGER }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_3() {
      val text = """
          pair Pair ::= { x 5, y 13 }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_4() {
      val text = """
          couple Couple ::= pair
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_5() {
      val text = """
          Lighter-state ::= ENUMERATED {
            on(0), off(1), out-of-order(2)
          }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_6() {
      val text = """
          Kettle-state ::= ENUMERATED {
            on(0), off(1), out-of-order(2)
          }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_6_1() {
      val text = """
          ENUMERATED {
            on(0), off(1),
            out-of-order(2)
          }
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_6_1_1() {
      val text = """
          ENUMERATED {
            on(0), off(1), out-of-order(2)
          }
          """
      parse(builtinType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_6_1_1x1() {
      val text = """
          ENUMERATED {
            on(0), off(1),
            out-of-order(2)
          }
          """
      parse(referencedType, text) match {
        case Success(_, _) => fail("Parse failure expected")
        case x => ()
      }
    }
    
    @Test def test_3_6_1_1x2() {
      val text = """
          ENUMERATED {
            on(0), off(1), out-of-order(2)
          }
          """
      parse(constrainedType, text) match {
        case Success(_, _) => fail("Parse failure expected")
        case x => ()
      }
    }
    
    @Test def test_3_6_1_1_1() {
      val text = """
          ENUMERATED {
            on(0), off(1), out-of-order(2)
          }
          """
      parse(enumeratedType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_6_1_1_1_1() {
      val text = """
          on(0), off(1), out-of-order(2)
          """
      parse(enumerations, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_6_1_1_1_1_1() {
      val text = """
          on(0), off(1), out-of-order(2)
          """
      parse(rootEnumeration, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_6_1_1_1_1_1_1() {
      val text = """
          on(0), off(1), out-of-order(2)
          """
      parse(enumeration, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_6_1_1_1_1_1_1_1() {
      val text = """
          on(0)
          """
      parse(enumerationItem, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_7() {
      val text = """
          lighter Lighter-state ::= on
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_8() {
      val text = """
          kettle Kettle-state ::= lighter
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4() {
      val text = """
          PrimeNumbers INTEGER ::= { 2 | 3 | 5 | 7 | 11 | 13 }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1() {
      val text = """
          PrimeNumbers INTEGER ::= { 2 | 3 | 5 | 7 | 11 | 13 }
          """
      parse(valueSetTypeAssignment, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1_1() {
      val text = """
          { 2 | 3 | 5 | 7 | 11 | 13 }
          """
      parse(valueSet, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1_1_1() {
      val text = """
          2 | 3 | 5 | 7 | 11 | 13
          """
      parse(elementSetSpecs, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1_1_1_1() {
      val text = """
          2 | 3 | 5 | 7 | 11 | 13
          """
      parse(rootElementSetSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1_1_1_1_1() {
      val text = """
          2 | 3 | 5 | 7 | 11 | 13
          """
      parse(elementSetSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1_1_1_1_1_1() {
      val text = """
          2 | 3 | 5 | 7 | 11 | 13
          """
      parse(unions, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1_1_1_1_1_1_1() {
      val text = """
          2
          """
      parse(intersections, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1_1_1_1_1_1_1_1() {
      val text = """
          2
          """
      parse(intersectionElements, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1_1_1_1_1_1_1_1_1() {
      val text = """
          2
          """
      parse(elements, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1_1_1_1_1_1_1_1_1_1() {
      val text = """
          2
          """
      parse(subtypeElements, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1_1_1_1_1_1_1_1_1_1_1() {
      val text = """
          2
          """
      parse(singleValue, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1_1_1_1_1_1_1_1_1_1_1_1() {
      val text = """
          2
          """
      parse(value, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1_1_1_1_1_1_1_1_1_1_1_1_1() {
      val text = """
          2
          """
      parse(builtinValue, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1_1_1_1_1_1_1_1_1_1_1_1_1_1() {
      val text = """
          2
          """
      parse(integerValue, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1_1_1_1_1_1_1_1_1_1_1_1_1_1x1() {
      val text = """
          2
          """
      parse(bitStringValue, text) match {
        case Success(_, _) => fail("Parse failure expected")
        case x => ()
      }
    }
    
    @Test def test_4_1_1_1_1_1_1_1_1_1_1_1_1_1_1x2() {
      val text = """
          2
          """
      parse(booleanValue, text) match {
        case Success(_, _) => fail("Parse failure expected")
        case x => ()
      }
    }
    
    @Test def test_4_1_1_1_1_1_1_1_1_1_1_1_1_1_1x3() {
      val text = """
          2
          """
      parse(characterStringValue, text) match {
        case Success(_, _) => fail("Parse failure expected")
        case x => ()
      }
    }
    
    @Test def test_4_1_1_1_1_1_1_1_1_1_1_1_1_1_1x4() {
      val text = """
          2
          """
      parse(choiceValue, text) match {
        case Success(_, _) => fail("Parse failure expected")
        case x => ()
      }
    }
    
    @Test def test_4_1_1_1_1_1_1_1_1_1_1_1_1_1_1x5() {
      val text = """
          2
          """
      parse(embeddedPdvValue, text) match {
        case Success(_, _) => fail("Parse failure expected")
        case x => ()
      }
    }
    
    @Test def test_4_1_1_1_1_1_1_1_1_1_1_1_1_1_1x6() {
      val text = """
          2
          """
      parse(enumeratedValue, text) match {
        case Success(_, _) => fail("Parse failure expected")
        case x => ()
      }
    }
    
    @Test def test_4_1_1_1_1_1_1_1_1_1_1_1_1_1_1x7() {
      val text = """
          2
          """
      parse(externalValue, text) match {
        case Success(_, _) => fail("Parse failure expected")
        case x => ()
      }
    }
    
    @Test def test_4_1_1_1_1_1_1_1_1_1_1_1_1_1_1x8() {
      val text = """
          2
          """
      parse(instanceOfValue, text) match {
        case Success(_, _) => fail("Parse failure expected")
        case x => ()
      }
    }
  }
}
