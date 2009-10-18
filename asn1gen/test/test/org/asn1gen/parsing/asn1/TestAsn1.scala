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
      Asn1.parse("MyModule DEFINITIONS AUTOMATIC TAGS") match {
        case Asn1.Success(ModuleDefinition(ModuleReference("MyModule")), _) =>
        case x => fail("Parse failed: " + x)
      }
    }

    @Test def test2() {
      Asn1.parse("MyModule   DEFINITIONS   AUTOMATIC   TAGS") match {
        case Asn1.Success(ModuleDefinition(ModuleReference("MyModule")), _) =>
        case x => fail("Parse failed: " + x)
      }
    }

    @Test def test3() {
      Asn1.parse("-- hello -- MyModule DEFINITIONS AUTOMATIC TAGS") match {
        case Asn1.Success(ModuleDefinition(ModuleReference("MyModule")), _) =>
        case x => fail("Parse failed: " + x)
      }
    }
  }
}
