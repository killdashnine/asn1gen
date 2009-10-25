import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1 {
  class TestParserT extends TestCase {
    
    object TheParser extends Parser {
      def parse[N](root : Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_type_1() {
      parse(`type`, "INTEGER { a(1), b(2), c(3) }") match {
        case Success(
          result@Type(_),
          _) =>
        case x => fail("Parse 'type_' failure: " + x)
      }
    }
    
    @Test def test_type_2() {
      parse(`type`, "CHOICE { choice1 [0] INTEGER, choice2 [1] INTEGER }") match {
        case Success(
          result@Type(
            BuiltinType(
              ChoiceType(
                AlternativeTypeLists(
                  RootAlternativeTypeList(
                    List(
                      NamedType(
                        Identifier("choice1"),
                        Type(
                          BuiltinType(
                            DefaultTaggedType(
                              Tag(DefaultClass(), LiteralClassNumber(Number("0"))),
                              Type(
                                BuiltinType(IntegerType(List()))))))),
                      NamedType(
                        Identifier("choice2"),
                        Type(
                          BuiltinType(
                            DefaultTaggedType(
                              Tag(DefaultClass(), LiteralClassNumber(Number("1"))),
                              Type(BuiltinType(IntegerType(List()))))))))),
                  None,None,None))))
,
          _) => println(result)
        case x => fail("Parse 'type_' failure: " + x)
      }
    }
    
    @Test def test_typeAssignment_1() {
      parse(typeAssignment, "hello ::= CHOICE { choice1 [0] INTEGER, choice2 [1] INTEGER }") match {
        case Success(
          result,
          _) =>
        case x => fail("Parse 'type' failure: " + x)
      }
    }
  }
}
