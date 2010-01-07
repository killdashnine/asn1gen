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
      MyChoice.Choice0(f(this))

    def choice1(f: (MyChoice => AsnInteger)): MyChoice =
      MyChoice.Choice1(f(this))

    def choice2(f: (MyChoice => AsnReal)): MyChoice =
      MyChoice.Choice2(f(this))
  }

  object MyChoice {
    object default extends Choice0(AsnNull)
    
    case class Choice0(_element: AsnNull) extends MyChoice {
      def _choice: Int = 0
      
      override def choice0: Option[AsnNull] = Some(_element)
    }
  
    case class Choice1(_element: AsnInteger) extends MyChoice {
      def _choice: Int = 1
      
      override def choice1: Option[AsnInteger] = Some(_element)
    }
  
    case class Choice2(_element: AsnReal) extends MyChoice {
      def _choice: Int = 2
      
      override def choice2: Option[AsnReal] = Some(_element)
    }
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
  
  trait MyPackratBerDecoder extends PackratBerDecoder {
    type MySequence
    
    def mySequence(length: Int): Parser[MySequence] =
      ( getOffset
      >>{ offset =>
          val wallOffset = offset + length
          val wall = offsetWall(wallOffset)
          ( ( (rawTagHeader >> where(_.tagType == 0))
            ~>(rawLength >> asnInteger) <~ wall
            ).?
          ~ ( (rawTagHeader >> where(_.tagType == 1))
            ~>(rawLength >> asnReal) <~ wall
            )
          ~ ( (rawTagHeader >> where(_.tagType == 2))
            ~>(rawLength >> asnPrintableString) <~ wall
            )
          ~ ( (rawTagHeader >> where(_.tagType == 3))
            ~>(rawLength >> myChoice) <~ wall
            )
          ) <~ atOffset(wallOffset)
        }
      ) ^^ mkMySequence
    
    def mkMySequence(data: Option[AsnInteger] ~ AsnReal ~ AsnPrintableString ~ MyChoice): MySequence
    
    def myChoice(length: Int): Parser[MyChoice] =
      ( getOffset
      >>{ offset =>
          val wallOffset = offset + length
          ( ( (rawTagHeader >> where(_.tagType == 0))
            ~> (rawLength >> asnNull)
            ) ^^ mkMyChoice_choice0
          ) <~ atOffset(wallOffset)
        }
      )

    def mkMyChoice_choice0(data: AsnNull): MyChoice
  }
  
  trait MyPackratBerRealiser extends PackratBerRealiser with Parsers {
    type MySequence = (Option[Long], Double, AsnPrintableString, MyChoice)
    type MyChoice = test.asn1.genruntime.MyChoice
    
    def mkMySequence(value: Option[Long] ~ Double ~ AsnPrintableString ~ MyChoice): MySequence = {
      value match { case a ~ b ~ c ~ d => (a, b, c, d) }
    }

    def mkMyChoice_choice0(data: AsnNull): MyChoice = {
      test.asn1.genruntime.MyChoice.Choice0(data)
    }
  }
}
