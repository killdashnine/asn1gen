import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1 {
  class TestParserK extends TestCase {
    
    object TheParser extends Parser {
      def parse[N](root : Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_me() {
    }
  }
}
