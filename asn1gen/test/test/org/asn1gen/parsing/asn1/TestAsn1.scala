import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1._
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1 {
  class TestAsn1 extends TestCase {
    @Test def test1() {
      def result = Asn1.parse("MyModule DEFINITIONS AUTOMATIC TAGS");
      assertEquals(ModuleDefinition(ModuleReference("MyModule")), result.get);
    }

    @Test def test2() {
      def result = Asn1.parse("_modulereference_");
      assertEquals(None, result);
    }
  }
}
