package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.syntax.Asn1Tokens
import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional

class Node extends Positional {
  object Tokens extends Asn1Tokens {}
  
  def withPosition(pos: Position): Node = {
    this.pos = pos
    return this
  }
}
