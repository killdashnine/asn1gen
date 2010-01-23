package test.org.asn1gen.gen.scala

import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1.Asn1Parser
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._
import _root_.org.asn1gen.gen.scala.AnonymousTypeNamer
import _root_.org.asn1gen.gen.scala.GenScalaAst
import _root_.org.asn1gen.io._

class TestAnonymousTypeNamer extends TestCase {
  object TheParser extends Asn1Parser {
    def parse[N](root: Parser[N], input: String) =
      phrase(root)(new lexical.Scanner(input))
  }
  
  import TheParser._
  
  @Test def test_1() {
    val text = """
      ParentType ::= SEQUENCE {
        field SEQUENCE {
          subField INTEGER
        }
      }
      """
    val sanityAssignments =
      AssignmentList(
        List(
          TypeAssignment(
            TypeReference("ParentType"),
            Type(
              SequenceType(
                ComponentTypeLists(
                  Some(
                    ComponentTypeList(
                      List(
                        NamedComponentType(
                          NamedType(
                            Identifier("field"),
                            Type(
                              SequenceType(
                                ComponentTypeLists(
                                  Some(
                                    ComponentTypeList(
                                      List(
                                        NamedComponentType(
                                          NamedType(
                                            Identifier("subField"),
                                            Type(INTEGER(None), List())),
                                          Empty)))),
                                  None,
                                  None)),
                              List())),
                          Empty)))),
                  None,
                  None)),
              List()))))

    parse(assignmentList, text) match {
      case Success(assignments, _) => {
        if (assignments != sanityAssignments) {
          GenScalaAst.generate(new IndentWriter(System.out), assignments)
          assert(false)
        }
        val processedAssignments = AnonymousTypeNamer.process(assignments)
        val indentWriter = new IndentWriter(System.out)
        GenScalaAst.generate(indentWriter, processedAssignments)
        indentWriter.flush()
      }
      case x => fail("Parse failure: " + x)
    }
  }
}
