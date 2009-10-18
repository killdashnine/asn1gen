import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1._
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._

package test.org.asn1gen.parsing.asn1 {
  class TestAsn1 extends TestCase {
    import Asn1.{parse, Failure, Success}

    @Test def test1() {
      Asn1.parse("MyModule DEFINITIONS AUTOMATIC TAGS") match {
        case Success(x, _) => { println(x)}
        case x => fail("Parse failed: " + x)
      }
    }
  }
}
