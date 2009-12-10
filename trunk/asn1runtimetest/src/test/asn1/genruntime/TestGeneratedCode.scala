package ModuleName {
  import org.asn1gen.runtime._

  case class MySequence(
    field1: AsnInteger,
    field2: AsnInteger
  ) extends AsnSequence {
    def field1(f: (AsnInteger => AsnInteger)): MySequence = MySequence(
      f(this.field1),
      this.field2)
    def field2(f: (AsnInteger => AsnInteger)): MySequence = MySequence(
      this.field1,
      f(this.field2))
  }
  case class MyChoice(override val choice: AsnType) extends AsnChoice(choice) {
    //////////////////////////////////////////////////////////////////
    // Choice IDs

    val CHOICE1: Integer = 0

    val CHOICE2: Integer = 1

    val CHOICE3: Integer = 2

    def choice1: AsnInteger = choice_.asInstanceOf[AsnInteger]

    def choice2: AsnReal = choice_.asInstanceOf[AsnReal]

    def choice3: MySequence = choice_.asInstanceOf[MySequence]
  }
}