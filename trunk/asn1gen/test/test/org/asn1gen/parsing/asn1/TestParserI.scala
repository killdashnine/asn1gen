import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1 {
  class TestParserI extends TestCase {
    
    object TheParser extends Parser {
      def parse[N](root : Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_identifier_1() {
      parse(identifier, "abcdef") match {
        case Success(Identifier("abcdef"), _) => 
        case x => fail("Parse 'identifier' failure: " + x)
      }
    }

    @Test def test_identifier_2() {
      parse(identifier, "abc-DEF") match {
        case Success(Identifier("abc-DEF"), _) => 
        case x => fail("Parse 'identifier' failure: " + x)
      }
    }

    @Test def test_identifier_3() {
      parse(identifier, "ABCDEF") match {
        case Success(Identifier("ABCDEF"), _) => fail("Parse success for 'identifier' unexpected: ")
        case x => 
      }
    }

    @Test def test_integerType() {
      parse(integerType, "INTEGER") match {
        case Success(result, _) => 
        case x => fail("Parse 'integerType' failure")
      }
    }
  }
}
