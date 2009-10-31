import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1 {
  class TestParser extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_parse_1() {
      parse(typeReference, "typeReference") match {
        case Success(result, _) => { fail("Must not succeed") }
        case x => {}
      };
    }
    
    @Test def test_parse_2() {
      parse(typeReference, "TypeReference") match {
        case Success(TypeReference(_), _) => {}
        case x => { fail("Parse failed: " + x) }
      };
    }

    @Test def test_word_1() {
      parse(word, "WORD") match {
        case Success(Word("WORD"), _) => {}
        case x => { fail("Parse failed: " + x) }
      };
    }

    @Test def test_word_2() {
      parse(word, "word") match {
        case Success(Word("_"), _) => fail("Parser failure expected")
        case x =>
      };
    }

    @Test def test_word_3() {
      parse(word, "WOrD") match {
        case Success(Word("_"), _) => fail("Parser failure expected")
        case x =>
      }
    }
    
    // ASN1D 9.2.2<2>
    @Test def test_moduleIdentifier_1() {
      parse(moduleIdentifier, "MyModule") match {
        case Success(ModuleIdentifier(ModuleReference("MyModule"), _), _) =>
        case x => fail("Parser failure expected")
      }
    } // TODO: cover other cases.

    @Test def test_kw_definitions() {
      parse(kw("DEFINITIONS"), "DEFINITIONS") match {
        case Success(Keyword("DEFINITIONS"), _) =>
        case x => fail("Parse failed: " + x)
      }
    } // TODO: cover other cases.

    @Test def test_op_assignment() {
      parse(op("::="), "::=") match {
        case Success(Operator("::="), _) =>
        case x => fail("Parse failed: " + x)
      }
    } // TODO: cover other cases.
  }
}
