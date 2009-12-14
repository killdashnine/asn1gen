package test.asn1.genruntime {
  import org.asn1gen.runtime._

  case class Empty() extends AsnSequence {
  }

  object Empty extends Empty() {
  }
}
package test.asn1.genruntime {
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

  object MySequence extends MySequence(Some(AsnInteger), AsnReal, AsnPrintableString, MyChoice) {
  }
}
package test.asn1.genruntime {
  import org.asn1gen.runtime._

  case class MyChoice(_choice: Int, _element: AsnType) extends AsnChoice {
    //////////////////////////////////////////////////////////////////
    // Choice IDs
    object _Choices {
      val field0: Int = 0
      val field1: Int = 1
      val field2: Int = 2
    }

    def choice0: Empty = _element.asInstanceOf[Empty]
    
    def choice1: AsnInteger = _element.asInstanceOf[AsnInteger]

    def choice2: AsnReal = _element.asInstanceOf[AsnReal]
    
    def field0(f: ((Int, AsnType) => Empty)): MyChoice = MyChoice(
      _Choices.field1, f(_choice, _element))
    
    def field1(f: ((Int, AsnType) => AsnInteger)): MyChoice = MyChoice(
      _Choices.field1, f(_choice, _element))
    
    def field2(f: ((Int, AsnType) => AsnReal)): MyChoice = MyChoice(
      _Choices.field2, f(_choice, _element))
  }

  object MyChoice extends MyChoice(0, Empty)
}
package test.asn1.genruntime {
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
