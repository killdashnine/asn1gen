import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch11 {
  class TestS10S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
          latinCapitalLetterA UniversalString ::= {0,0,0,65}
          greekCapitalLetterSigma UniversalString ::= {0,0,3,145}
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_2() {
      val text = """
          MyModule DEFINITIONS ::=
            BEGIN
              IMPORTS
                latinCapitalLetterA, greekCapitalLetterAlpha
                  FROM ASN1-CHARACTER-MODULE
                    {joint-iso-itu-t asn1(1) specification(0) modules(0) iso10646(0)};
              my-string UniversalString ::= {
                "This is a capital A: ",
                latinCapitalLetterA,
                ", and a capital alpha: ",
                greekCapitalLetterAlpha,
                "; try and spot the difference!"}
            END
          """
      parse(moduleDefinition, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_3() {
      val text = """
          Latin1Level1 ::= UniversalString (FROM (Latin1 INTERSECTION Level1))
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_4() {
      val text = """
          C0 ::= UniversalString (FROM ({0,0,0,0}..{0,0,0,31}))
          C1 ::= UniversalString (FROM ({0,0,0,128}..{0,0,0,159}))
          VanillaUniversalString ::= UniversalString (FROM (ALL EXCEPT (C0|C1)))
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_4_1() {
      val text = """
          C0 ::= UniversalString (FROM ({0,0,0,0}..{0,0,0,31}))
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_4_2() {
      val text = """
          C1 ::= UniversalString (FROM ({0,0,0,128}..{0,0,0,159}))
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_4_3() {
      val text = """
          VanillaUniversalString ::= UniversalString (FROM (ALL EXCEPT (C0|C1)))
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_4_3_1() {
      val text = """
          UniversalString (FROM (ALL EXCEPT (C0|C1)))
          """
      parse(_type, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_4_3_1_1() {
      val text = """
          (FROM (ALL EXCEPT (C0|C1)))
          """
      parse(constraint, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_4_3_1_1_1() {
      val text = """
          FROM (ALL EXCEPT (C0|C1))
          """
      parse(permittedAlphabet, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_4_3_1_1_1_1() {
      val text = """
          (ALL EXCEPT (C0|C1))
          """
      parse(constraint, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_4_3_1_1_1_1_1() {
      val text = """
          ALL EXCEPT (C0|C1)
          """
      parse(elementSetSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_4_3_1_1_1_1_1_1x() {
      val text = """
          ALL EXCEPT (C0|C1)
          """
      parse(unions, text) match {
        case Success(_, _) => fail("Parse failure expected")
        case x => ()
      }
    }

    @Test def test_4_3_1_1_1_1_1_2() {
      val text = """
          ALL EXCEPT (C0|C1)
          """
      parse(kw("ALL") ~ exclusions, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_5() {
      val text = """
          ISO10646String {
            UniversalString:ImplementationSubset,
            UniversalString:ImplementationLevel } ::=
              UniversalString (
                FROM ((ImplementationSubset UNION BasicLatin) INTERSECTION ImplementationLevel)
                !characterSetProblem)
            characterSetProblem INTEGER ::= 1
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }

    @Test def test_6() {
      val text = """
          MyLevel2String ::= ISO10646String{ {HebrewExtended UNION Hiragana}, {Level2} }
          """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
