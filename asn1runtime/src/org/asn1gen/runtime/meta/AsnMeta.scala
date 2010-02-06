package org.asn1gen.runtime.meta

trait AsnMeta {
  def name: String
  
  def children: Map[String, AsnMember] = Map.empty
}
