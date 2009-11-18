import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch11 {
  class TestS12S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
          MyModule DEFINITIONS ::=
            BEGIN
              IMPORTS latinCapitalLetterA, greekCapitalLetterAlpha
                FROM ASN1-CHARACTER-MODULE
                  {joint-iso-itu-t asn1(1) specification(0) modules(0) iso10646(0)};
              my-string UTF8String ::=
                { "This is a capital A: "
                , latinCapitalLetterA
                , ", and a capital alpha: "
                , greekCapitalLetterAlpha
                , "; spot the difference!"
                }
            END
          """
      parse(moduleDefinition, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
