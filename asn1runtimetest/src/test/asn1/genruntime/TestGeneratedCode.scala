package test.asn1.genruntime {
  import org.asn1gen.runtime._
  import org.asn1gen.runtime.codec._

  case class Empty() extends AsnSequence {
  }

  object Empty extends Empty() {
  }

  import org.asn1gen.runtime._

  case class MySequence(
    field1: Option[AsnInteger],
    field2: AsnReal,
    field3: AsnPrintableString,
    field4: MyChoice
  ) extends AsnSequence {
    def field1(f: (Option[AsnInteger] => Option[AsnInteger])): MySequence = MySequence(
      f(this.field1),
      this.field2,
      this.field3,
      this.field4)
    def field2(f: (AsnReal => AsnReal)): MySequence = MySequence(
      this.field1,
      f(this.field2),
      this.field3,
      this.field4)
    def field3(f: (AsnPrintableString => AsnPrintableString)): MySequence = MySequence(
      this.field1,
      this.field2,
      f(this.field3),
      this.field4)
    def field4(f: (MyChoice => MyChoice)): MySequence = MySequence(
      this.field1,
      this.field2,
      this.field3,
      f(this.field4))
  }

  object MySequence extends MySequence(
    Some(AsnInteger),
    AsnReal,
    AsnPrintableString,
    MyChoice
  ) {
  }

  import org.asn1gen.runtime._

  abstract class MyChoice(_element: AsnType) extends AsnChoice {
    def _choice: Int

    def choice0: Empty = _element.asInstanceOf[Empty]

    def choice1: AsnInteger = _element.asInstanceOf[AsnInteger]

    def choice2: AsnReal = _element.asInstanceOf[AsnReal]

    def choice0(f: (MyChoice => Empty)): MyChoice =
      MyChoice_choice0(f(this))

    def choice1(f: (MyChoice => AsnInteger)): MyChoice =
      MyChoice_choice1(f(this))

    def choice2(f: (MyChoice => AsnReal)): MyChoice =
      MyChoice_choice2(f(this))
  }

  case class MyChoice_choice0(_element: Empty) extends MyChoice(_element) {
    def _choice: Int = 0
  }

  case class MyChoice_choice1(_element: AsnInteger) extends MyChoice(_element) {
    def _choice: Int = 1
  }

  case class MyChoice_choice2(_element: AsnReal) extends MyChoice(_element) {
    def _choice: Int = 2
  }

  object MyChoice extends MyChoice_choice0(Empty) {
  }

  import org.asn1gen.runtime._

  case class MyEnum(_value: Int) extends AsnEnumeration {
  }

  object MyEnum extends MyEnum(0) {
    def value0: MyEnum = MyEnum(0)
    def value1: MyEnum = MyEnum(1)
    def value2: MyEnum = MyEnum(2)
    def value3: MyEnum = MyEnum(3)
  }
  
  class RepeatingTripletDecoder(is: DecodingInputStream, endIndex: Int) extends BerDecoder {
    var triplet: Option[Triplet] = None
    
    def decode(f: PartialFunction[Option[Triplet], Unit]) = {
      assert(is.index <= endIndex)
      if (triplet == None && is.index < endIndex) {
        triplet = Some(decodeTriplet(is))
      }
      val result = f.lift(triplet)
      if (result != None) {
        triplet = None
      }
    }
  }

  trait BerDecoder extends org.asn1gen.runtime.codec.BerDecoderBase {
    def decodeTriplets(is: DecodingInputStream, length: Int)(f: RepeatingTripletDecoder => Unit): Unit = {
      val newIndex = is.index + length
      f(new RepeatingTripletDecoder(is, newIndex))
      println(is.index + " vs " + newIndex + " vs " + length)
      assert(is.index == newIndex)
    }
  }
  
  /*object MySequenceWindow {
    def decode[T](is: DecodingInputStream)(ff: (Option[AsnIntegerRaw], Double, String, MyChoiceWindow) => T): T = {
      new MySequenceWindow(is).decode(ff)
    }
  }*/
  
  /*class MySequenceHandler(field0: AsnIntegerHandler) {
  }*/
  
  /*class AsnIntegerHandler {
    def apply(is: DecodingInputStream)(handler: Long => Unit): Unit = {
      val value =
        if (length == 0) {
          0
        } else {
          val buffer = new Array[Byte](length)
          is.read(buffer)
          var acc = if (buffer(0) >= 0) 0L else -1L
          buffer foreach { byte =>
            acc = (acc << 8) | byte
          }
          acc
        }
      handler(value)
    }
  }*/
  /*
  class MySequenceWindow(is: DecodingInputStream) extends BerDecoder {
    def decode(handlers: MySequenceHandler): Unit = {
      val triplet = decodeTriplet(is)
      var field0: Option[Boolean] = None
      var field1: Option[Double] = None
      var field2: Option[String] = None
      var field3: Option[MyChoiceWindow] = None
      decodeTriplets(is, triplet.length) { tripletsDecoder =>
        tripletsDecoder.decode {
          case Some(triplet) if triplet.tagType == 0 =>
            handlers.field0(is)
            Some(true)
          case None => None
        }
        tripletsDecoder.decode {
          case Some(triplet) if triplet.tagType == 1 => field1 = Some(0)
        }
        tripletsDecoder.decode {
          case Some(triplet) if triplet.tagType == 2 => field2 = Some("")
        }
        tripletsDecoder.decode {
          case Some(triplet) if triplet.tagType == 3 => field3 = Some(new MyChoiceWindow(is))
        }
      }
    }
  }
  
  class MyChoiceWindow(is: DecodingInputStream) extends BerDecoder {
    def decode[T](ff: (Empty, Int, Double) => T): T = {
      val triplet = decodeTriplet(is)
      var fieldTriplet = decodeTriplet(is)
      var field0: Option[Empty] = None
      var field1: Option[Int] = None
      var field2: Option[Double] = None
      decodeTriplets(is, triplet.length) { tripletsDecoder =>
        tripletsDecoder.decode {
          case Some(triplet) if triplet.tagType == 0 => field0 = Some(Empty)
          case None => None
        }
        tripletsDecoder.decode {
          case Some(triplet) if triplet.tagType == 1 => field1 = Some(0)
        }
        tripletsDecoder.decode {
          case Some(triplet) if triplet.tagType == 2 => field2 = Some(0.0)
        }
      }
      return ff(field0.get, field1.get, field2.get)
    }
  }*/
  
  object BerDecoder extends BerDecoder
  
  //case class OnMySequence(field0: )
  
  trait Decodable {
    def decode(is: DecodingInputStream, length: Int): Unit
  }
  
  case class OnAsnInteger(value: Long => Unit) extends Decodable {
    def value(transform: (Long => Unit) => (Long => Unit)): OnAsnInteger =
      OnAsnInteger(transform(this.value))
    
    def decode(is: DecodingInputStream, length: Int): Unit = {
      val intValue =
        if (length == 0) {
          0
        } else {
          val buffer = new Array[Byte](length)
          val bytesRead = is.read(buffer)
          assert(bytesRead == length)
          var acc = if (buffer(0) >= 0) 0L else -1L
          buffer foreach { byte =>
            acc = (acc << 8) | byte
          }
          acc
        }
      val action: Long => Unit = this.value
      action(intValue)
    }
  }
  
  object OnAsnInteger extends OnAsnInteger({_=>}){
  }
  
  case class OnMySequence(field0: OnAsnInteger) extends BerDecoder with Decodable {
    def field0(transform: OnAsnInteger => OnAsnInteger): OnMySequence =
      OnMySequence(transform(this.field0))
    
    def decode(is: DecodingInputStream, length: Int): Unit = {
      decodeTriplets(is, length) { tripletsDecoder =>
        tripletsDecoder.decode {
          case Some(triplet) if triplet.tagType == 0 =>
            this.field0.decode(is, triplet.length)
            Some(true)
          case None => None
        }
      }
    }
  }
  
  object OnMySequence extends OnMySequence(OnAsnInteger)
}
