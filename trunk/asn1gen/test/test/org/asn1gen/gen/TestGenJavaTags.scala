import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1._
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.gen {
  class TestGenJavaTags extends TestCase {
    import Asn1._
    
    @Test def test1() {
      val text = """
        ModuleName DEFINITIONS ::= BEGIN MyChoice ::= CHOICE { choice1 [0] INTEGER, choice2 [1] INTEGER } END
        """
      Asn1.parse(text) match {
        case Asn1.Success(
          moduleDefinition@ModuleDefinition(
            ModuleIdentifier(
              ModuleReference("ModuleName"),
              DefinitiveIdentifier()),
            TagDefault(),
            ExtensionDefault(),
            ModuleBody(_, _, _)), _) => println(moduleDefinition)
        case x => fail("Parse failed: " + x)
      }
    }
  }
}
