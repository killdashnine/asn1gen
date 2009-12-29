package test.org.asn1gen.scratch

import org.junit._

class TestExceptionPerformance {
  def measure(msg: String)(f: => Unit): Unit = {
    val before = System.currentTimeMillis
    f
    val after = System.currentTimeMillis
    println(msg + ": " + (after - before))
  }
  
  @Test
  def test_noexceptions_01(): Unit = {
    var collection = new scala.collection.mutable.Stack[Int]()
    def a(i: Int) {
      if (i > 0) a(i - 1) else 0
      collection.push(i)
      if (collection.size > 100) {
        collection = new scala.collection.mutable.Stack[Int]()
      }
    }
    
    measure("noexceptions") {
      val before = 1
      0 to 100000 foreach { i =>
        a(100)
      }
    }
  }

  @Test
  def test_exceptions_01(): Unit = {
    def noop() {
      throw new Exception()
    }
    def a(i: Int) {
      if (i > 0) a(i - 1) else 0
      noop()
    }
    
    measure("exceptions") {
      val before = 1
      0 to 100000 foreach { i =>
        try {
          a(100)
        } catch { case _ =>
        }
      }
    }
  }

  @Test
  def test_sameexception_01(): Unit = {
    val exception = new Exception()
    def noop() {
      //throw exception
    }
    def a(i: Int) {
      if (i > 0) a(i - 1) else 0
      noop()
    }
    
    measure("sameexception") {
      val before = 1
      0 to 100000 foreach { i =>
        try {
          a(100)
        } catch { case _ =>
        }
      }
    }
  }
}
