import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1.ch15 {
  class TestS03S1 extends TestCase {
    
    object TheParser extends Asn1Parser {
      def parse[N](root: Parser[N], input: String) =
        phrase(root)(new lexical.Scanner(input))
    }
    
    import TheParser._
    
    @Test def test_1() {
      val text = """
        OTHER-FUNCTION ::= CLASS {
          &code  INTEGER (0..MAX) UNIQUE,
          &Alphabet  BMPString DEFAULT {Latin1 INTERSECTION Level1},
          &ArgumentType,
          &SupportedArguments &ArgumentType OPTIONAL,
          &ResultType DEFAULT NULL,
          &result-if-error &ResultType DEFAULT NULL,
          &associated-function OTHER-FUNCTION OPTIONAL,
          &Errors ERROR DEFAULT
          {rejected-argument|memory-fault}
        } WITH SYNTAX {
          ARGUMENT TYPE &ArgumentType,
          [SUPPORTED ARGUMENTS &SupportedArguments,]
          [RESULT TYPE &ResultType, [RETURNS &result-if-error IN CASE OF ERROR,]]
          [ERRORS &Errors,]
          [MESSAGE ALPHABET &Alphabet,]
          [ASSOCIATED FUNCTION &associated-function,]
          CODE &code
        }
        memory-fault ERROR ::= {-- object definition --}
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_1() {
      val text = """
        OTHER-FUNCTION ::= CLASS {
          &code  INTEGER (0..MAX) UNIQUE,
          &Alphabet  BMPString DEFAULT {Latin1 INTERSECTION Level1},
          &ArgumentType,
          &SupportedArguments &ArgumentType OPTIONAL,
          &ResultType DEFAULT NULL,
          &result-if-error &ResultType DEFAULT NULL,
          &associated-function OTHER-FUNCTION OPTIONAL,
          &Errors ERROR DEFAULT
          {rejected-argument|memory-fault}
        } WITH SYNTAX {
          ARGUMENT TYPE &ArgumentType,
          [SUPPORTED ARGUMENTS &SupportedArguments,]
          [RESULT TYPE &ResultType, [RETURNS &result-if-error IN CASE OF ERROR,]]
          [ERRORS &Errors,]
          [MESSAGE ALPHABET &Alphabet,]
          [ASSOCIATED FUNCTION &associated-function,]
          CODE &code
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_1_1() {
      val text = """
        CLASS {
          &code  INTEGER (0..MAX) UNIQUE,
          &Alphabet  BMPString DEFAULT {Latin1 INTERSECTION Level1},
          &ArgumentType,
          &SupportedArguments &ArgumentType OPTIONAL,
          &ResultType DEFAULT NULL,
          &result-if-error &ResultType DEFAULT NULL,
          &associated-function OTHER-FUNCTION OPTIONAL,
          &Errors ERROR DEFAULT
          {rejected-argument|memory-fault}
        } WITH SYNTAX {
          ARGUMENT TYPE &ArgumentType,
          [SUPPORTED ARGUMENTS &SupportedArguments,]
          [RESULT TYPE &ResultType, [RETURNS &result-if-error IN CASE OF ERROR,]]
          [ERRORS &Errors,]
          [MESSAGE ALPHABET &Alphabet,]
          [ASSOCIATED FUNCTION &associated-function,]
          CODE &code
        }
      """
      parse(objectClassDefn, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_1_1_1() {
      val text = """
        &code  INTEGER (0..MAX) UNIQUE,
        &Alphabet  BMPString DEFAULT {Latin1 INTERSECTION Level1},
        &ArgumentType,
        &SupportedArguments &ArgumentType OPTIONAL,
        &ResultType DEFAULT NULL,
        &result-if-error &ResultType DEFAULT NULL,
        &associated-function OTHER-FUNCTION OPTIONAL,
        &Errors ERROR DEFAULT
        {rejected-argument|memory-fault}
      """
      parse(rep1sep(fieldSpec, op(",")), text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_1_1_1_1() {
      val text = """
        &code  INTEGER (0..MAX) UNIQUE
      """
      parse(fieldSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_1_1_1_2() {
      val text = """
        &Alphabet  BMPString DEFAULT {Latin1 INTERSECTION Level1}
      """
      parse(fieldSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_1_1_1_3() {
      val text = """
        &ArgumentType
      """
      parse(fieldSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_1_1_1_4() {
      val text = """
        &SupportedArguments &ArgumentType OPTIONAL
      """
      parse(fieldSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_1_1_1_5() {
      val text = """
        &ResultType DEFAULT NULL
      """
      parse(fieldSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_1_1_1_6() {
      val text = """
        &result-if-error &ResultType DEFAULT NULL
      """
      parse(fieldSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_1_1_1_7() {
      val text = """
        &associated-function OTHER-FUNCTION OPTIONAL
      """
      parse(fieldSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_1_1_1_8() {
      val text = """
        &Errors ERROR DEFAULT {rejected-argument|memory-fault}
      """
      parse(fieldSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_1_1_2() {
      val text = """
        WITH SYNTAX {
          ARGUMENT TYPE &ArgumentType,
          [SUPPORTED ARGUMENTS &SupportedArguments,]
          [RESULT TYPE &ResultType, [RETURNS &result-if-error IN CASE OF ERROR,]]
          [ERRORS &Errors,]
          [MESSAGE ALPHABET &Alphabet,]
          [ASSOCIATED FUNCTION &associated-function,]
          CODE &code
        }
      """
      parse(withSyntaxSpec, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_1_2() {
      val text = """
        memory-fault ERROR ::= {-- object definition --}
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
    
    @Test def test_2() {
      val text = """
        addition-of-2-integers OTHER-FUNCTION ::= {
          ARGUMENT TYPE Pair,
          SUPPORTED ARGUMENTS {PosPair | NegPair},
          RESULT TYPE INTEGER,
          RETURNS 0 IN CASE OF ERROR,
          CODE 1
        }
      """
      parse(assignmentList, text) match {
        case Success(_, _) => ()
        case x => fail("Parse failure: " + x)
      }
    }
  }
}
