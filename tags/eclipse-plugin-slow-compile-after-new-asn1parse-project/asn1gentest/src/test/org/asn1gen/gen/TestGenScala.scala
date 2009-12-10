import _root_.org.junit._
import _root_.org.junit.Assert._
import _root_.junit.framework.TestCase
import _root_.org.asn1gen.parsing.asn1._
import _root_.org.asn1gen.parsing.asn1.ast._
import _root_.scala.util.parsing.input._
import _root_.org.asn1gen.gen._
import _root_.org.asn1gen.io.IndentWriter

package test.org.asn1gen.gen {
  class TestGenScala extends TestCase {
    import Asn1._
    
    @Test def test1() {
      val text = """
        ModuleName DEFINITIONS ::= BEGIN -- xasdfsfd -- MyChoice ::= CHOICE {
          choice1 [0] INTEGER,
          choice2 [1] REAL
        } END
        """
      Asn1.parse(text) match {
        case Asn1.Success(moduleDefinition, _) => {
          new GenScala(new IndentWriter(System.out)).generate(moduleDefinition)
        }
        case x => fail("Parse failed: " + x)
      }
    }
    
    @Test def test2() {
      val text = """
        ModuleName DEFINITIONS ::= BEGIN -- xasdfsfd -- MySequence ::= SEQUENCE {
          field1 [0] INTEGER,
          field2 [1] REAL
        } END
        """
      Asn1.parse(text) match {
        case Asn1.Success(moduleDefinition, _) => {
          new GenScala(new IndentWriter(System.out)).generate(moduleDefinition)
        }
        case x => fail("Parse failed: " + x)
      }
    }
    
    @Test def test3() {
      val text = """
        ModuleName DEFINITIONS ::= BEGIN
	      MySequence ::= SEQUENCE {
	        field1 [0] INTEGER,
	        field2 [1] INTEGER
	      }
	      MyChoice ::= CHOICE {
            choice1 [0] INTEGER,
            choice2 [1] REAL,
            choice3 [2] MySequence
          }
        END
        """
      Asn1.parse(text) match {
        case Asn1.Success(moduleDefinition, _) => {
          new GenScala(new IndentWriter(System.out)).generate(moduleDefinition)
        }
        case x => fail("Parse failed: " + x)
      }
    }
  }
}
