package test.asn1.genruntime {
  import org.asn1gen.runtime._
  import org.asn1gen.runtime.codec._
  import org.asn1gen.runtime.codec.async._
  import scala.util.parsing.combinator.Parsers

  import org.asn1gen.runtime._

  case class MySequence(
    field0: Option[AsnInteger],
    field1: AsnReal,
    field2: AsnPrintableString,
    field3: MyChoice
  ) extends AsnSequence {
    def field0(f: (Option[AsnInteger] => Option[AsnInteger])): MySequence = copy(field0 = f(field0))
    def field1(f: (AsnReal => AsnReal)): MySequence = copy(field1 = f(field1))
    def field2(f: (AsnPrintableString => AsnPrintableString)): MySequence = copy(field2 = f(field2))
    def field3(f: (MyChoice => MyChoice)): MySequence = copy(field3 = f(field3))
  }

  object MySequence extends MySequence(
    Some(AsnInteger),
    AsnReal,
    AsnPrintableString,
    MyChoice.default
  ) {
  }

  import org.asn1gen.runtime._

  abstract class MyChoice extends AsnChoice {
    def _choice: Int

    def choice0: Option[AsnNull] = None

    def choice1: Option[AsnInteger] = None

    def choice2: Option[AsnReal] = None

    def choice0(f: (MyChoice => AsnNull)): MyChoice =
      MyChoice_Choice0(f(this))

    def choice1(f: (MyChoice => AsnInteger)): MyChoice =
      MyChoice_Choice1(f(this))

    def choice2(f: (MyChoice => AsnReal)): MyChoice =
      MyChoice_Choice2(f(this))
  }
  
  case class MyChoice_Choice0(_element: AsnNull) extends MyChoice {
    def _choice: Int = 0
    
    override def choice0: Option[AsnNull] = Some(_element)
  }

  case class MyChoice_Choice1(_element: AsnInteger) extends MyChoice {
    def _choice: Int = 1
    
    override def choice1: Option[AsnInteger] = Some(_element)
  }

  case class MyChoice_Choice2(_element: AsnReal) extends MyChoice {
    def _choice: Int = 2
    
    override def choice2: Option[AsnReal] = Some(_element)
  }

  object MyChoice {
    object default extends MyChoice_Choice0(AsnNull)
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
  
  trait BerDecoder extends org.asn1gen.runtime.codec.BerDecoderBase {
    def decodeTriplets(is: DecodingInputStream, length: Int)(f: RepeatingTripletDecoder => Unit): Unit = {
      val newIndex = is.index + length
      f(new RepeatingTripletDecoder(is, newIndex))
      assert(is.index == newIndex)
    }
  }
  
  object BerDecoder extends BerDecoder
  
  case class OnMySequence(field0: OnAsnInteger, field1: OnAsnInteger) extends BerDecoder with Decodable {
    def field0(transform: OnAsnInteger => OnAsnInteger): OnMySequence =
      OnMySequence(transform(this.field0), this.field1)
    
    def field1(transform: OnAsnInteger => OnAsnInteger): OnMySequence =
      OnMySequence(this.field0, transform(this.field1))
    
    def decode(is: DecodingInputStream, length: Int): Unit = {
      decodeTriplets(is, length) { tripletsDecoder =>
        tripletsDecoder.decode {
          case Some(triplet) if triplet.tagType == 0 =>
            this.field0.decode(is, triplet.length)
            Some(true)
          case None => None
        }
        tripletsDecoder.decode {
          case Some(triplet) if triplet.tagType == 1 =>
            this.field1.decode(is, triplet.length)
            Some(true)
          case None => None
        }
      }
    }
  }
  
  object OnMySequence extends OnMySequence(OnAsnInteger, OnAsnInteger)
  
  import org.asn1gen.runtime.codec.PackratBerDecoder
}
