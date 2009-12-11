package test.asn1.genruntime {
  import org.asn1gen.runtime._

  case class MySequence(
    field1: AsnInteger,
    field2: AsnReal,
    field3: AsnPrintableString,
    field4: MyChoice
  ) extends AsnSequence {
    def field1(f: (AsnInteger => AsnInteger)): MySequence = MySequence(
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
    // Unmatched type: Type_(TypeReference(MyChoice),List())
  }
}
package test.asn1.genruntime {
  import org.asn1gen.runtime._

  case class MyChoice(override val choice: AsnType) extends AsnChoice(choice) {
    //////////////////////////////////////////////////////////////////
    // Choice IDs

    val CHOICE1: Integer = 0

    val CHOICE2: Integer = 1

    def choice1: AsnInteger = choice_.asInstanceOf[AsnInteger]

    def choice2: AsnReal = choice_.asInstanceOf[AsnReal]
  }
}
