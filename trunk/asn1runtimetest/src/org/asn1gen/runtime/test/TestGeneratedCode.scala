package org.asn1gen.runtime.test {
  import org.asn1gen.runtime._

  case class Empty() extends AsnSequence {
  }

  object Empty extends Empty() {
  }
}
package org.asn1gen.runtime.test {
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
}
package org.asn1gen.runtime.test {
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
}
package org.asn1gen.runtime.test {
  import org.asn1gen.runtime._

  case class MyEnum(_value: Int) extends AsnEnumeration {
  }

  object MyEnum extends MyEnum(0) {
    def value0: MyEnum = MyEnum(0)
    def value1: MyEnum = MyEnum(1)
    def value2: MyEnum = MyEnum(2)
    def value3: MyEnum = MyEnum(3)
  }
}
package org.asn1gen.runtime.test {
  trait BerDecoder extends org.asn1gen.runtime.codec.BerDecoderBase
  
  object BerDecoder extends BerDecoder
}
