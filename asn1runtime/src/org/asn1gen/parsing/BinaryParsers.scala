package org.asn1gen.parsing

import scala.util.parsing.combinator._
import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble

trait BinaryParsers extends Parsers with ParsersUtil {
  type Elem = Byte

  protected implicit def readerToByteReader(x: Input): ByteReader = x match {
    case br: ByteReader => br
    case _ => new ByteReader(x)
  }

  override def acceptIf(p: Elem => Boolean)(err: Elem => String): Parser[Elem] = Parser { in =>
    try {
      if (p(in.first)) {
        Success(in.first, in.rest)
      } else {
        Failure(err(in.first), in)
      }
    } catch {
      case e if e eq EofException => Failure("EOF unexpected", in)
    }
  }
  
  override def acceptMatch[U](expected: String, f: PartialFunction[Elem, U]): Parser[U] = Parser{ in =>
    try {
      if (f.isDefinedAt(in.first)) {
        Success(f(in.first), in.rest)
      } else {
        Failure(expected + " expected", in)
      }
    } catch {
      case e if e eq EofException => Failure("EOF unexpected: " + expected + " expected", in)
    }
  }
  
  def toInt(bytes: Seq[Byte]): Int = bytes.foldLeft(0)((x, b) => (x << 8) + (b & 0xFF))
  def toLong(bytes: Seq[Byte]): Long = bytes.foldLeft(0L)((x, b) => (x << 8) + (b & 0xFF))

  lazy val byte: Parser[Byte] = anyElem
  lazy val u1: Parser[Int] = byte ^^ (_ & 0xFF)
  lazy val u2: Parser[Int] = bytes(2) ^^ toInt
  lazy val u4: Parser[Int] = bytes(4) ^^ toInt
  lazy val u4f: Parser[Float] = u4 ^^ intBitsToFloat
  lazy val u8: Parser[Long] = bytes(8) ^^ toLong
  lazy val u8d: Parser[Double] = u8 ^^ longBitsToDouble

  def bytes(n: Int): Parser[Seq[Byte]] = Parser { in =>
     if (n <= in.length) Success(in take n, in drop n)
     else Failure("Requested %d bytes but only %d remain".format(n, in.length), in)
  }

  def parse[T](p: Parser[T], in: Input): ParseResult[T] = p(in)
  def parse[T](p: Parser[T], in: String): ParseResult[T] = parse(p, new ByteReader(in))
}
