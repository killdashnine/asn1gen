package org.asn1gen.parsing

import scala.util.parsing.combinator._

trait ParsersUtil extends Parsers {
  lazy val anyElem: Parser[Elem] = elem("anyElem", {_ => true})
  def elemExcept(xs: Elem*): Parser[Elem] = elem("elemExcept", x => !(xs contains x))
  def elemOf(xs: Elem*): Parser[Elem] = elem("elemOf", xs contains _)

  def take(n: Int): Parser[Seq[Elem]] = repN(n, anyElem)
  def takeUntil(cond: Parser[Elem]): Parser[Seq[Elem]] = takeUntil(cond, anyElem)
  def takeUntil(cond: Parser[Elem], p: Parser[Elem]): Parser[Seq[Elem]] = rep(not(cond) ~> p)
  def takeWhile(p: Parser[Elem]): Parser[Seq[Elem]] = rep(p)
  
  def offsetWall(value: Int): Parser[Unit] = Parser { in =>
    if (value - in.offset < 0) {
      Success((), in)
    } else {
      Failure("Offset wall exceeded", in)
    }
  }
  
  def offset(value: Int): Parser[Unit] = Parser { in =>
    if (value == in.offset) {
      Success((), in)
    } else {
      Failure("Not at expected offset", in)
    }
  }
}
