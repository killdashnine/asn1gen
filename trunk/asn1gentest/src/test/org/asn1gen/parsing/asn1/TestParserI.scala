import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1 {
  class TestParserI extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
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

    @Test def test_integerType_1() {
      parse(integerType, "INTEGER") match {
        case Success(result@IntegerType(Nil), _) => 
        case x => fail("Parse 'integerType' failure")
      }
    }
    
    @Test def test_integerType_2() {
      parse(integerType, "INTEGER { a(1), b(2), c(3) }") match {
        case Success(
          result@IntegerType(
            List(
              NamedNumber(Identifier("a"), SignedNumber(false, Number("1"))),
              NamedNumber(Identifier("b"), SignedNumber(false, Number("2"))),
              NamedNumber(Identifier("c"), SignedNumber(false, Number("3"))))),
          _) =>
        case x => fail("Parse 'integerType' failure")
      }
    }
  }
}
