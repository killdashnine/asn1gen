import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1._
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._
import _root_.org.asn1gen.gen._
import _root_.org.asn1gen.gen.java._
import _root_.org.asn1gen.io.IndentWriter

package test.org.asn1gen.gen {
  class TestGenJavaTags extends TestCase {
    import Asn1._
    
    @Test def test1() {
      val text = """
        ModuleName3 DEFINITIONS ::= BEGIN MyChoice2 ::= CHOICE { choice1 [1] INTEGER, choice2 [5] INTEGER } END
        """
      Asn1.parse(text) match {
        case Asn1.Success(moduleDefinition, _) => {
          println(moduleDefinition)
          new GenJavaChoiceIds(new IndentWriter(System.out)).generate(moduleDefinition)
        }
        case x => fail("Parse failed: " + x)
      }
    }
  }
}
