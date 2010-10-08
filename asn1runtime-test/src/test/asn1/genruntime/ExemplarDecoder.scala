package test.asn1.genruntime

import org.asn1gen.runtime.codec._

trait ExemplarDecoder extends PackratBerDecoder {
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
        | ( (rawTagHeader >> where(_.tagType == 1))
          ~> (rawLength >> asnInteger)
          ) ^^ mkMyChoice_choice1
        | ( (rawTagHeader >> where(_.tagType == 2))
          ~> (rawLength >> asnReal)
          ) ^^ mkMyChoice_choice2
        ) <~ atOffset(wallOffset)
      }
    )

  def mkMyChoice_choice0(data: AsnNull): MyChoice
  def mkMyChoice_choice1(data: AsnInteger): MyChoice
  def mkMyChoice_choice2(data: AsnReal): MyChoice
}
