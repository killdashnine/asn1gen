import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch10 {
  class TestS8S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
          EDIBodyPartType ::= OBJECT IDENTIFIER
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
          internet-id OBJECT IDENTIFIER ::= { iso(1) identified-organization(3) dod(6) internet(1) }
          francetelecom-id OBJECT IDENTIFIER ::= { iso member-body f(250) type-org(1) ft(16) }
          ber-id OBJECT IDENTIFIER ::= { 2 1 1 }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1() {
      val text = """
          internet-id OBJECT IDENTIFIER ::= { iso(1) identified-organization(3) dod(6) internet(1) }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1() {
      val text = """
          internet-id OBJECT IDENTIFIER ::= { iso(1) identified-organization(3) dod(6) internet(1) }
          """
      parse(valueAssignment, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1_1() {
      val text = """
          { iso(1) identified-organization(3) dod(6) internet(1) }
          """
      parse(value, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1_1_1() {
      val text = """
          { iso(1) identified-organization(3) dod(6) internet(1) }
          """
      parse(builtinValue, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_1_1_1_1_1() {
      val text = """
          { iso(1) identified-organization(3) dod(6) internet(1) }
          """
      parse(objectIdentifierValue, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_2() {
      val text = """
          francetelecom-id OBJECT IDENTIFIER ::= { iso member-body f(250) type-org(1) ft(16) }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2_3() {
      val text = """
          ber-id OBJECT IDENTIFIER ::= { 2 1 1 }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_3() {
      val text = """
          ID ::= OBJECT IDENTIFIER
          id-edims ID ::= { joint-iso-itu-t mhs-motif(6) edims(7) }
          id-bp ID ::= { id-edims 11 }
          id-bp-edifact-ISO646 ID ::= { id-bp 1 }
          id-bp-edifact-T61 ID ::= { id-bp 2 }
          id-bp-edifact-octet ID ::= { id-bp 3 }
          id-bp-ansiX12-ISO646 ID ::= { id-bp 4 }
          id-bp-ansiX12-T61 ID ::= { id-bp 5 }
          id-bp-ansiX12-ebcdic ID ::= { id-bp 6 }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_4() {
      val text = """
          ber-descriptor ObjectDescriptor ::= "Basic Encoding of a single ASN.1 type"
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_5() {
      val text = """
          ModuleName { iso member-body(2) f(250) type-org(1) ft(16) asn1-book(9) chapter10(2) module0(0) }
          DEFINITIONS ::=
            BEGIN
              I ::= INTEGER
            END
          """
      parse(moduleDefinition, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_6() {
      val text = """
          Homonym { iso member-body(2) f(250) type-org(1) ft(16) asn1-book(9) chapter10(2) homonym1(1) }
          DEFINITIONS ::=
            BEGIN
              T ::= INTEGER
            END
          """
      parse(moduleDefinition, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_7() {
      val text = """
          Homonym { iso member-body(2) f(250) type-org(1) ft(16) asn1-book(9) chapter10(2) homonym2(2) }
          DEFINITIONS ::=
            BEGIN
              T ::= REAL
              U ::= BOOLEAN
            END
          """
      parse(moduleDefinition, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_8() {
      val text = """
          Module1 DEFINITIONS ::=
            BEGIN
              IMPORTS
                T FROM Homonym {
                  iso member-body(2)
                  type-org(1) ft(16) asn1-book(9)
                  chapter10(2) homonym1(1) }
                U FROM Homonym {
                  iso member-body(2) type-org(1) ft(16) asn1-book(9)
                  chapter10(2) homonym2(2) } ;
              V ::= SEQUENCE { integer T, boolean U }
            END
          """
      parse(moduleDefinition, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_9() {
      val text = """
          Module2 DEFINITIONS ::=
            BEGIN
              IMPORTS
                T FROM Homonym {
                  iso member-body(2)
                  type-org(1) ft(16) asn1-book(9) chapter10(2) homonym1(1) }
                T FROM Surname -- renaming -- {
                  iso f(250) member-body(2)
                  f(250) type-org(1) ft(16) asn1-book(9)
                  chapter10(2) homonym2(2) };
              W ::= CHOICE { integer Homonym.T, real  Surname.T -- local name --}
            END
          """
      parse(moduleDefinition, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
