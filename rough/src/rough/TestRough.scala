import moo.AMPTYPES._

package rough {
  object TestRough {
    def main(args: Array[String]): Unit = {
      println("Hello world")
      val secBoardId1 =
        ( AmpSecBoardId
            .secCode { _ => AmpSecurityCode("xyz".getBytes.toList) }
        )
      val secBoardId2 =
        ( AmpSecBoardId
            .secCode { _ => AmpSecurityCode("xyz".getBytes.toList) }
        )
      val secBoardId3 =
        ( AmpSecBoardId
            .secCode { _ => AmpSecurityCode("abc".getBytes.toList) }
        )
      println(secBoardId1)
      println(secBoardId1.secCode)
      println(secBoardId1.boardId)
      println(secBoardId2)
      println(secBoardId2.secCode)
      println(secBoardId2.boardId)
      println("?")
      println(secBoardId1 == secBoardId2)
      println(secBoardId1 == secBoardId3)
      println(secBoardId1._desc.name)
      println(secBoardId1._desc.children)
      secBoardId1._desc.children.foreach { case (key, value) =>
        println(key + ": " + value)
      }
      //println(secBoardId1.inspect())
    }
  }
}
