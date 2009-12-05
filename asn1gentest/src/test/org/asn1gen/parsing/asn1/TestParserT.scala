import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1 {
  class TestParserT extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_type_1() {
      parse(type_, "INTEGER { a(1), b(2), c(3) }") match {
        case Success(
          result@Type_(_, _),
          _) =>
        case x => fail("Parse 'type_' failure: " + x)
      }
    }
    
    @Test def test_type_2() {
      parse(type_, "CHOICE { choice1 [0] INTEGER, choice2 [1] INTEGER }") match {
        case Success(
          result@Type_(
            ChoiceType(
              AlternativeTypeLists(
                RootAlternativeTypeList(
                  AlternativeTypeList(
                    List(
                      NamedType(
                        Identifier("choice1"),
                        Type_(
                          TaggedType(
                            Tag(Empty, ClassNumber(Number("0"))),
                            _,
                            Type_(
                              IntegerType(None), _)),
                          _)),
                      NamedType(
                        Identifier("choice2"),
                        Type_(
                          TaggedType(
                            Tag(Empty, ClassNumber(Number("1"))),
                            _,
                            Type_(IntegerType(None), _)),
                          _))))),
                  None,None,None)),
            _),
          _) => println(result)
        case x => fail("Parse 'type_' failure: " + x)
      }
    }
    
    @Test def test_typeAssignment_1() {
      parse(typeAssignment, "MyChoice ::= CHOICE { choice1 [0] INTEGER, choice2 [1] INTEGER }") match {
        case Success(
          result,
          _) =>
        case x => fail("Parse 'type' failure: " + x)
      }
    }
    
    // 9.1.1
    @Test def test_typeAssignment_2() {
      parse(typeAssignment, "TypeReference ::= CHOICE { integer INTEGER, boolean BOOLEAN }") match {
        case Success(
          TypeAssignment(
            TypeReference("TypeReference"),
            Type_(ChoiceType(AlternativeTypeLists(
              RootAlternativeTypeList(
                AlternativeTypeList(
                  List(
                    NamedType(Identifier("integer"),Type_(IntegerType(None), _)),
                    NamedType(Identifier("boolean"),Type_(BOOLEAN, _))))),
              None,
              None,
              None)),
              _)),
          _) =>
        case x => fail("Parse 'type' failure: " + x)
      }
    }
    
    // 9.1.1
    @Test def test_typeAssignment_3() {
      parse(typeAssignment, "Pair ::= SEQUENCE { x INTEGER, y INTEGER }") match {
        case Success(
          result@TypeAssignment(TypeReference("Pair"), Type_(SequenceType(), _)),
          _) =>
        case x => fail("Parse 'type' failure: " + x)
      }
    }
  }
}
