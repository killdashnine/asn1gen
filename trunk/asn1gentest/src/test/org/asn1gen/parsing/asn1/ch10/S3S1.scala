import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch10 {
  class TestS3S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """" +
          zero INTEGER ::= 0
          australian-population INTEGER ::= 20000000
          fridge-temperature INTEGER ::= -18 -- in Centigrade scale
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
          Interval ::= INTEGER (123..456)
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3() {
      val text = """
          ErrorCode ::= INTEGER {
            disk-full(1),
            no-disk(-1),
            disk-not-formatted(2)
          }

          disk-is-full ErrorCode ::= disk-full
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4() {
      val text = """
          ok ErrorCode ::= 0
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5() {
      val text = """" +
          disk-is-full ErrorCode ::= 1 -- disk full
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_6() {
      val text = """
          IO-error ::= INTEGER {
            file-not-found(0),
            access-denied(1),
            disk-full(2),
            file-not-shared(3),
            file-exists(4)
          }
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_7() {
      val text = """
          Centigrade ::= INTEGER { freezing(0), boiling(100) }
          """
      parse(type_, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
