package test.org.asn1gen.runtime.codec

import org.junit.runner.RunWith

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class TestSpecs extends org.specs2.mutable.SpecificationWithJUnit {
  "'hello world' has 11 characters" in {
     "hello world".size must_== 11
  }
  "'hello world' matches 'h.* w.*'" in {
     "hello world" must be matching("h.* w.*")
  }
}
