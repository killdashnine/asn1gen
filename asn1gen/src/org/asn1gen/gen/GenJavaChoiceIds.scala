package org.asn1gen.gen

import java.io.PrintWriter
import org.asn1gen.parsing.asn1.ast._

class GenJavaChoiceIds(out: PrintWriter) {
  
  def generate(moduleDefinition: ModuleDefinition): Unit = {
    moduleDefinition match {
      case moduleDefinition@ModuleDefinition(
        ModuleIdentifier(
          ModuleReference("ModuleName"),
          DefinitiveIdentifier()),
        TagDefault(),
        ExtensionDefault(),
        ModuleBody(_, _, tal))
      => println(tal)
    }
  }
}
