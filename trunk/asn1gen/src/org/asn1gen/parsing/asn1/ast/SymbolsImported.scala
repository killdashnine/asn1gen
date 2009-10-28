package org.asn1gen.parsing.asn1.ast

case class SymbolsImported(
  symbolsFromModuleList: List[SymbolsFromModule]
) extends Node {
}

