package org.asn1gen.parsing.asn1.ast

case class SymbolsFromModule(
  symbols: List[Symbol],
  globalModuleReference: GlobalModuleReference
) extends Node {
}
