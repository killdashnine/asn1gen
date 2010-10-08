import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch09 {
  class TestS2S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
          ModuleName DEFINITIONS ::=
            BEGIN
              I ::= INTEGER -- assignments
            END
          """
      parse(root, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
          ModuleName { iso member-body(2) a(1) b-c(2) de-fg(3) hi-jk(4) l(5) m(6) }
          DEFINITIONS ::=
            BEGIN
              I ::= INTEGER -- assignments
            END
          """
      parse(root, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1() {
      val text = """
          { iso member-body(2) a(1) b-c(2) de-fg(3) hi-jk(4) l(5) m(6) }
          """
      parse(definitiveIdentifier, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3() {
      val text = """
          ModuleName
          DEFINITIONS ::=
          BEGIN
            IMPORTS
              Type1 FROM Module1 { iso member-body(2) a(1) b-c(2) de-fg(3) hi-jk(4) l(5) m(6) }
              value2 FROM Module2 { iso member-body(2) a(1) b-c(2) de-fg(3) hi-jk(4) l(5) m(6) };
            I ::= INTEGER -- assignments
          END
          """
      parse(root, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_1() {
      val text = """
          Module1 { iso member-body(2) a(1) b-c(2) de-fg(3) hi-jk(4) l(5) m(6) }
          """
      parse(globalModuleReference, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_1_1() {
      val text = """
          { iso member-body(2) a(1) b-c(2) de-fg(3) hi-jk(4) l(5) m(6) }
          """
      parse(assignedIdentifier, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_1_1_1() {
      val text = """
          { iso member-body(2) a(1) b-c(2) de-fg(3) hi-jk(4) l(5) m(6) }
          """
      parse(objectIdentifierValue, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_1_1_1_1() {
      val text = """
          iso
          """
      parse(definedValue, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3_1_1_1_2() {
      val text = """
          a(1)
          """
      parse(objIdComponents, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4() {
      val text = """
          ModuleName
          DEFINITIONS ::=
          BEGIN
            IMPORTS T{} FROM Module1;
            
            U ::= T{INTEGER}
          END
          """
      parse(root, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1() {
      val text = """
          IMPORTS T{} FROM Module1;
          """
      parse(imports, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1_1() {
      val text = """
          T{} FROM Module1
          """
      parse(symbolsFromModule, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1_1_1() {
      val text = """
          T{}
          """
      parse(symbol, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_1_1_1_1() {
      val text = """
          T{}
          """
      parse(parameterizedReference, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_2() {
      val text = """
          U ::= T{INTEGER}
          """
      parse(typeAssignment, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_2_1() {
      val text = """
          T{INTEGER}
          """
      parse(_type, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_2_1_1() {
      val text = """
          T{INTEGER}
          """
      parse(definedType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4_2_1_1_1() {
      val text = """
          T{INTEGER}
          """
      parse(parameterizedType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5() {
      val text = """
          ModuleName DEFINITIONS ::=
          BEGIN
            EXPORTS Type1, Type2;
            IMPORTS Type1, value1 FROM Module1;
            
            Type2 ::= SET { b BOOLEAN, a Type1 DEFAULT value1 }
          END
          """
      parse(root, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5_1() {
      val text = """
          SET { b BOOLEAN, a Type1 DEFAULT value1 }
          """
      parse(setType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5_1_1() {
      val text = """
          a Type1 DEFAULT value1
          """
      parse(componentType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5_1_1_1() {
      val text = """
          a Type1
          """
      parse(namedType, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5_1_1_2() {
      val text = """
          value1
          """
      parse(value, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_6() {
      val text = """
          ModuleName DEFINITIONS AUTOMATIC TAGS EXTENSIBILITY IMPLIED ::=
          BEGIN
            I ::= INTEGER -- assignments
          END
          """
      parse(root, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
