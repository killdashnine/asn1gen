package test.org.asn1gen.runtime.codec

import org.specs._
import org.specs.runner._

class TestSpecs extends JUnit4(TestSpecs)

object TestSpecs extends Specification with JUnit {
  "'hello world' has 11 characters" in {
     "hello world".size must_== 11
  }
  "'hello world' matches 'h.* w.*'" in {
     "hello world" must be matching("h.* w.*")
  }
}
