package org.asn1gen.parsing.asn1.ast

case class Default[+T](value: T) extends Node with OptionalDefault[T] {
}
