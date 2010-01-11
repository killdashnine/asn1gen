package org.asn1gen.parsing.asn1.ast

import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional

class Node extends Positional {
  def withPosition(pos: Position): Node = {
    this.pos = pos
    return this
  }
}
