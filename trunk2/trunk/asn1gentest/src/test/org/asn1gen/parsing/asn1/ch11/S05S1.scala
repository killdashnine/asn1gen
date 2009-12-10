import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch11 {
  class TestS05S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
          cr IA5String ::= {0,13}
          del IA5String ::= {7,15}
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
          ExampleIA5String DEFINITIONS ::=
            BEGIN
              IMPORTS
                cr FROM ASN1-CHARACTER-MODULE
                  {joint-iso-itu-t asn1(1) specification(0) modules(0) iso10646(0)} ;
              two-lines IA5String ::= { "First line", cr, "Second line" }
            END
          """
      parse(moduleDefinition, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
