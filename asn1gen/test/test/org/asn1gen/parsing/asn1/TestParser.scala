import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1 {
  class TestParser extends TestCase {
    
    object TheParser extends Parser {
      def parse[N](root : Parser[N], input: String) =
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
  }
}
