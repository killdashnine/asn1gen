import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1._
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1 {
  class TestAsn1 extends TestCase {
    import Asn1._

    @Test def test1() {
      val text = """
        ModuleName DEFINITIONS ::= BEGIN END
        """
      Asn1.parse(text) match {
        case Asn1.Success(
          ModuleDefinition(
            ModuleIdentifier(
              ModuleReference("ModuleName"),
              DefinitiveIdentifier(_)),
            TagDefault(_),
            ExtensionDefault(_),
            ModuleBody(_, _, _)), _) =>
        case x => fail("Parse failed: " + x)
      }
    }

    @Test def test2() {
      Asn1.parse(" ModuleName    DEFINITIONS    ::= BEGIN  END") match {
        case Asn1.Success(
          ModuleDefinition(
            ModuleIdentifier(
              ModuleReference("ModuleName"),
              DefinitiveIdentifier(_)),
            TagDefault(_),
            ExtensionDefault(_),
            ModuleBody(_, _, _)), _) =>
        case x => fail("Parse failed: " + x)
      }
    }

    @Test def test3() {
      Asn1.parse("-- hello -- ModuleName DEFINITIONS ::= BEGIN Moo ::= INTEGER END") match {
        case Asn1.Success(
          md@ModuleDefinition(
            ModuleIdentifier(
              ModuleReference("ModuleName"),
              DefinitiveIdentifier(_)),
            TagDefault(_),
            ExtensionDefault(_),
            ModuleBody(
              _,
              _,
              AssignmentList(
                List(
                  Assignment(x))))), _) => println(md)
        case x => fail("Parse failed: " + x)
      }
    }
  }
}
