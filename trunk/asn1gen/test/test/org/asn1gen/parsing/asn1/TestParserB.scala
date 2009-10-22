import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1 {
  class TestParserB extends TestCase {
    
    object TheParser extends Parser {
      def parse[N](root : Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_booleanValue_1() {
      parse(booleanValue, "TRUE") match {
        case Success(
          result@BooleanValue(true),
          _) => println(result)
        case x => println(x); fail("Parse 'booleanValue' failure: " + x)
      }
    }
    
    @Test def test_booleanValue_2() {
      parse(booleanValue, "FALSE") match {
        case Success(
          result@BooleanValue(false),
          _) => println(result)
        case x => println(x); fail("Parse 'booleanValue' failure: " + x)
      }
    }
    
    @Test def test_builtinType_1() {
      parse(builtinType, "NULL") match {
        case Success(
          result@BuiltinType(),
          _) => println(result)
        case x => println(x); fail("Parse 'builtinType' failure: " + x)
      }
    }
    
    @Test def test_builtinType_2() {
      parse(builtinType, "INTEGER { a(1), b(2), c(3) }") match {
        case Success(
          result@BuiltinType(),
          _) => println(result)
        case x => println(x); fail("Parse 'builtinType' failure: " + x)
      }
    }
  }
}
