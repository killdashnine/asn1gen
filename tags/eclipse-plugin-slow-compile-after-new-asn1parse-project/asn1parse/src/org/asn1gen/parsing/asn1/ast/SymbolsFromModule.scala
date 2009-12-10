package org.asn1gen.parsing.asn1.ast

case class SymbolsFromModule(
  symbol: List[Symbol],
  globalModuleReference: GlobalModuleReference
) extends Node {
}

