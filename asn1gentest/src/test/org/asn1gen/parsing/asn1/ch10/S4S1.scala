import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch10 {
  class TestS4S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_01() {
      val text = """" +
          IO-error ::= ENUMERATED {
            file-not-found(1),
            access-denied(2),
            disk-full(3),
            file-not-shared(4),
            file-exists(5),
            ...
          }
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_02() {
      val text = """
          Direction ::= ENUMERATED {
            north(0),
            south(1),
            east(3),
            west(4)
          }

          default-direction Direction ::= north
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_03() {
      val text = """
          Direction ::= ENUMERATED { north, south, east, west }
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_04() {
      val text = """
          Direction ::= ENUMERATED { north, south, east, west, ... }
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_05() {
      val text = """" +
          Direction ::= ENUMERATED {
            north, south, east, west,
            ..., up, down
          }
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_06() {
      val text = """
          A ::= ENUMERATED { a, b(3), ..., c(1) }
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_07() {
      val text = """
          A ::= ENUMERATED { a, b, ..., c(2) }
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_08() {
      val text = """
          A ::= ENUMERATED { a, b, ..., c }
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_09() {
      val text = """
          ENUMERATED { a, b, ..., c(3), d }
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_10() {
      val text = """
          ChoiceOfNull ::= CHOICE { e1 NULL, e2 NULL, e3 NULL }
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
