package org.asn1gen.gen.java

import org.asn1gen.parsing.asn1.{ast => ast}

object NameOf {
  val keywords = Set("yield", "type", "null", "final")
  
  def asnTypeOf(namedComponentType: ast.NamedComponentType): String = {
    namedComponentType match {
      case ast.NamedComponentType(
        ast.NamedType(ast.Identifier(identifier), _type),
        value)
      => {
        asnTypeOf(_type, value)
      }
    }
  }
  
  def asnTypeOf(_type: ast.Type, value: ast.OptionalDefault[ast.Value]): String = {
    value match {
      case ast.Empty =>
        return asnTypeOf(_type)
      case ast.Default(value) =>
        return asnTypeOf(_type)
      case ast.Optional =>
        return "Option[" + asnTypeOf(_type) + "]"
    }
  }  

  def rawTypeOf(namedComponentType: ast.NamedComponentType): String = {
    namedComponentType match {
      case ast.NamedComponentType(
        ast.NamedType(ast.Identifier(identifier), _type),
        value)
      => {
        rawTypeOf(_type, value)
      }
    }
  }
  
  def rawTypeOf(_type: ast.Type, value: ast.OptionalDefault[ast.Value]): String = {
    value match {
      case ast.Empty =>
        return rawTypeOf(_type)
      case ast.Default(value) =>
        return rawTypeOf(_type)
      case ast.Optional =>
        return "Option[" + rawTypeOf(_type) + "]"
    }
  }

  def rawTypeOf(_type: ast.Type): String = {
    _type match {
      case ast.Type(typeKind, _) => rawTypeOf(typeKind)
    }
  }
  
  def rawTypeOf(typeKind: ast.TypeKind): String = {
    typeKind match {
      case builtinType: ast.BuiltinType => rawTypeOf(builtinType)
      case usefulType: ast.UsefulType => rawTypeOf(usefulType)
      case ast.TypeReference(reference) => reference
      case unmatched => "Unmatched(" + unmatched + ")"
    }
  }
  
  def rawTypeOf(typeKind: ast.TypeKind, value: ast.OptionalDefault[ast.Value]): String = {
    value match {
      case ast.Empty =>
        return rawTypeOf(typeKind)
      case ast.Default(value) =>
        return rawTypeOf(typeKind)
      case ast.Optional =>
        return "Option[" + rawTypeOf(typeKind) + "]"
    }
  }
  
  def rawTypeOf(builtinType: ast.UsefulType): String = {
    builtinType match {
      case ast.GeneralizedTime => {
        return "org.asn1gen.runtime.java.AsnGeneralizedTime"
      }
      case ast.ObjectDescriptor => {
        return "org.asn1gen.runtime.java.AsnObjectDescriptor"
      }
      case ast.UTCTime => {
        return "org.asn1gen.runtime.java.AsnUtcTime"
      }
      case unmatched => {
        return "UnknownUsefulType(" + unmatched + ")"
      }
    }
  }
  
  def rawTypeOf(builtinType: ast.BuiltinType): String = {
    builtinType match {
      case ast.BitStringType(_) => {
        return "String"
      }
      case ast.BOOLEAN => {
        return "Boolean"
      }
      case characterString: ast.CharacterStringType => {
        rawTypeOf(characterString)
      }
      case _: ast.ChoiceType => {
        return "org.asn1gen.runtime.java.AsnChoice"
      }
      case ast.EmbeddedPdvType => {
        return "org.asn1gen.runtime.java.AsnEmbeddedPdv"
      }
      case ast.EnumeratedType(_) => {
        return "org.asn1gen.runtime.java.AsnEnumeration"
      }
      case ast.EXTERNAL => {
        return "ExternalType"
      }
      case ast.InstanceOfType(_) => {
        return "InstanceOfType"
      }
      case ast.INTEGER(_) => {
        return "Long"
      }
      case ast.NULL => {
        return "org.asn1gen.runtime.java.AsnNull"
      }
      case _: ast.ObjectClassFieldType => {
        return "org.asn1gen.runtime.java.AsnObjectClassField"
      }
      case ast.ObjectIdentifierType => {
        return "org.asn1gen.runtime.java.AsnObjectIdentifier"
      }
      case ast.OctetStringType => {
        return "String"
      }
      case ast.REAL => {
        return "Double"
      }
      case ast.RelativeOidType => {
        return "org.asn1gen.runtime.java.AsnRelativeOidType"
      }
      case ast.SequenceOfType(_) => {
        return "org.asn1gen.runtime.java.AsnSequenceOf"
      }
      case ast.SequenceType(_) => {
        return "org.asn1gen.runtime.java.AsnSequence"
      }
      case ast.SetOfType(_) => {
        return "org.asn1gen.runtime.java.AsnSetOf"
      }
      case ast.SetType(_) => {
        return "org.asn1gen.runtime.java.AsnSet"
      }
      case ast.TaggedType(_, _, underlyingType) => {
        return rawTypeOf(underlyingType)
      }
      case unmatched => {
        return "UnknownBuiltinType(" + unmatched + ")"
      }
    }
  }
  
  def rawTypeOf(characterString: ast.CharacterStringType): String = {
    characterString match {
      case ast.BMPString => {
        return "org.asn1gen.runtime.java.AsnBmpString"
      }
      case ast.GeneralString => {
        return "org.asn1gen.runtime.java.AsnGeneralString"
      }
      case ast.GraphicString => {
        return "org.asn1gen.runtime.java.AsnGraphicString"
      }
      case ast.IA5String => {
        return "org.asn1gen.runtime.java.AsnIa5String"
      }
      case ast.ISO646String => {
        return "org.asn1gen.runtime.java.AsnIso646String"
      }
      case ast.NumericString => {
        return "org.asn1gen.runtime.java.AsnNumericString"
      }
      case ast.PrintableString => {
        return "String"
      }
      case ast.T61String => {
        return "org.asn1gen.runtime.java.AsnT61String"
      }
      case ast.TeletexString => {
        return "org.asn1gen.runtime.java.AsnTeletexString"
      }
      case ast.UniversalString => {
        return "org.asn1gen.runtime.java.AsnUniversalString"
      }
      case ast.UTF8String => {
        return "String"
      }
      case ast.VideotexString => {
        return "org.asn1gen.runtime.java.AsnVideotexString"
      }
      case ast.VisibleString => {
        return "org.asn1gen.runtime.java.AsnVisibleString"
      }
      case unknown => {
        return "UnknownCharacterString(" + unknown + ")"
      }
    }
  }
  
  
  ///
  
  def defaultNameOf(namedComponentType: ast.NamedComponentType): String = {
    namedComponentType match {
      case ast.NamedComponentType(
        ast.NamedType(ast.Identifier(identifier), _type),
        value)
      => {
        defaultNameOf(_type, value)
      }
    }
  }
  
  def defaultNameOf(_type: ast.Type, value: ast.OptionalDefault[ast.Value]): String = {
    value match {
      case ast.Empty =>
        return asnTypeOf(_type)
      case ast.Default(value) =>
        return defaultNameOf(_type)
      case ast.Optional =>
        return "None"
    }
  }

  ///////
  
  def rawDefaultOf(namedComponentType: ast.NamedComponentType): String = {
    namedComponentType match {
      case ast.NamedComponentType(
        ast.NamedType(ast.Identifier(identifier), _type),
        value)
      => {
        rawDefaultOf(_type, value)
      }
    }
  }
  
  def rawDefaultOf(_type: ast.Type, value: ast.OptionalDefault[ast.Value]): String = {
    value match {
      case ast.Empty =>
        return asnTypeOf(_type)
      case ast.Default(value) =>
        return rawDefaultOf(_type)
      case ast.Optional =>
        return "None"
    }
  }
  
  def defaultNameOf(_type: ast.Type): String = {
    _type match {
      case ast.Type(typeKind, _) => defaultNameOf(typeKind)
    }
  }
  
  def defaultNameOf(typeKind: ast.TypeKind): String = {
    typeKind match {
      case builtinType: ast.BuiltinType => defaultNameOf(builtinType)
      case usefulType: ast.UsefulType => defaultNameOf(usefulType)
      case ast.TypeReference(reference) => reference
      case unmatched => "UnmatchedDefaultName(" + unmatched + ")"
    }
  }
  
  def defaultNameOf(typeKind: ast.TypeKind, value: ast.OptionalDefault[ast.Value]): String = {
    value match {
      case ast.Empty =>
        return defaultNameOf(typeKind)
      case ast.Default(value) =>
        return defaultNameOf(typeKind)
      case ast.Optional =>
        return "None"
    }
  }
  
  def defaultNameOf(usefulType: ast.UsefulType): String = {
    usefulType match {
      case ast.GeneralizedTime => {
        return "org.asn1gen.runtime.java.AsnGeneralizedTime"
      }
      case ast.ObjectDescriptor => {
        return "org.asn1gen.runtime.java.AsnObjectDescriptor"
      }
      case ast.UTCTime => {
        return "org.asn1gen.runtime.java.AsnUtcTime"
      }
      case unmatched => {
        return "UnknownUsefulType(" + unmatched + ")"
      }
    }
  }
  
  def defaultNameOf(builtinType: ast.BuiltinType): String = {
    builtinType match {
      case ast.BitStringType(_) => {
        return "org.asn1gen.runtime.java.AsnBitString"
      }
      case ast.BOOLEAN => {
        return "org.asn1gen.runtime.java.AsnFalse"
      }
      case characterString: ast.CharacterStringType => {
        defaultNameOf(characterString)
      }
      case _: ast.ChoiceType => {
        return "org.asn1gen.runtime.java.AsnChoice"
      }
      case ast.EmbeddedPdvType => {
        return "org.asn1gen.runtime.java.AsnEmbeddedPdv"
      }
      case ast.EnumeratedType(_) => {
        return "org.asn1gen.runtime.java.AsnEnumeration"
      }
      case ast.EXTERNAL => {
        return "ExternalType"
      }
      case ast.InstanceOfType(_) => {
        return "InstanceOfType"
      }
      case ast.INTEGER(_) => {
        return "0L"
      }
      case ast.NULL => {
        return "org.asn1gen.runtime.java.AsnNull"
      }
      case _: ast.ObjectClassFieldType => {
        return "org.asn1gen.runtime.java.AsnObjectClassField"
      }
      case ast.ObjectIdentifierType => {
        return "org.asn1gen.runtime.java.AsnObjectIdentifier"
      }
      case ast.OctetStringType => {
        return "org.asn1gen.runtime.java.AsnOctetString"
      }
      case ast.REAL => {
        return "0.0"
      }
      case ast.RelativeOidType => {
        return "org.asn1gen.runtime.java.AsnRelativeOidType"
      }
      case ast.SequenceOfType(_) => {
        return "org.asn1gen.runtime.java.AsnSequenceOf"
      }
      case ast.SequenceType(_) => {
        return "org.asn1gen.runtime.java.AsnSequence"
      }
      case ast.SetOfType(_) => {
        return "org.asn1gen.runtime.java.AsnSetOf"
      }
      case ast.SetType(_) => {
        return "org.asn1gen.runtime.java.AsnSet"
      }
      case ast.TaggedType(_, _, underlyingType) => {
        return defaultNameOf(underlyingType)
      }
      case unmatched => {
        return "UnknownBuiltinType(" + unmatched + ")"
      }
    }
  }
  
  def defaultNameOf(characterString: ast.CharacterStringType): String = {
    characterString match {
      case ast.BMPString => {
        return "org.asn1gen.runtime.java.AsnBmpString"
      }
      case ast.GeneralString => {
        return "org.asn1gen.runtime.java.AsnGeneralString"
      }
      case ast.GraphicString => {
        return "org.asn1gen.runtime.java.AsnGraphicString"
      }
      case ast.IA5String => {
        return "org.asn1gen.runtime.java.AsnIa5String"
      }
      case ast.ISO646String => {
        return "org.asn1gen.runtime.java.AsnIso646String"
      }
      case ast.NumericString => {
        return "org.asn1gen.runtime.java.AsnNumericString"
      }
      case ast.PrintableString => {
        return "\"\""
      }
      case ast.T61String => {
        return "org.asn1gen.runtime.java.AsnT61String"
      }
      case ast.TeletexString => {
        return "org.asn1gen.runtime.java.AsnTeletexString"
      }
      case ast.UniversalString => {
        return "org.asn1gen.runtime.java.AsnUniversalString"
      }
      case ast.UTF8String => {
        return "\"\""
      }
      case ast.VideotexString => {
        return "org.asn1gen.runtime.java.AsnVideotexString"
      }
      case ast.VisibleString => {
        return "org.asn1gen.runtime.java.AsnVisibleString"
      }
      case unknown => {
        return "UnknownCharacterString(" + unknown + ")"
      }
    }
  }

  /////////
  
  def rawDefaultOf(_type: ast.Type): String = {
    _type match {
      case ast.Type(typeKind, _) => rawDefaultOf(typeKind)
    }
  }
  
  def rawDefaultOf(typeKind: ast.TypeKind): String = {
    typeKind match {
      case builtinType: ast.BuiltinType => rawDefaultOf(builtinType)
      case usefulType: ast.UsefulType => rawDefaultOf(usefulType)
      case ast.TypeReference(reference) => reference
      case unmatched => "UnmatchedDefaultName(" + unmatched + ")"
    }
  }
  
  def rawDefaultOf(typeKind: ast.TypeKind, value: ast.OptionalDefault[ast.Value]): String = {
    value match {
      case ast.Empty =>
        return rawDefaultOf(typeKind)
      case ast.Default(value) =>
        return rawDefaultOf(typeKind)
      case ast.Optional =>
        return "None"
    }
  }
  
  def rawDefaultOf(usefulType: ast.UsefulType): String = {
    usefulType match {
      case ast.GeneralizedTime => {
        return "org.asn1gen.runtime.java.AsnGeneralizedTime"
      }
      case ast.ObjectDescriptor => {
        return "org.asn1gen.runtime.java.AsnObjectDescriptor"
      }
      case ast.UTCTime => {
        return "org.asn1gen.runtime.java.AsnUtcTime"
      }
      case unmatched => {
        return "UnknownUsefulType(" + unmatched + ")"
      }
    }
  }
  
  def rawDefaultOf(builtinType: ast.BuiltinType): String = {
    builtinType match {
      case ast.BitStringType(_) => {
        return "org.asn1gen.runtime.java.AsnBitString"
      }
      case ast.BOOLEAN => {
        return "false"
      }
      case characterString: ast.CharacterStringType => {
        rawDefaultOf(characterString)
      }
      case _: ast.ChoiceType => {
        return "org.asn1gen.runtime.java.AsnChoice"
      }
      case ast.EmbeddedPdvType => {
        return "org.asn1gen.runtime.java.AsnEmbeddedPdv"
      }
      case ast.EnumeratedType(_) => {
        return "org.asn1gen.runtime.java.AsnEnumeration"
      }
      case ast.EXTERNAL => {
        return "ExternalType"
      }
      case ast.InstanceOfType(_) => {
        return "InstanceOfType"
      }
      case ast.INTEGER(_) => {
        return "0L"
      }
      case ast.NULL => {
        return "org.asn1gen.runtime.java.AsnNull"
      }
      case _: ast.ObjectClassFieldType => {
        return "org.asn1gen.runtime.java.AsnObjectClassField"
      }
      case ast.ObjectIdentifierType => {
        return "org.asn1gen.runtime.java.AsnObjectIdentifier"
      }
      case ast.OctetStringType => {
        return "\"\""
      }
      case ast.REAL => {
        return "0.0"
      }
      case ast.RelativeOidType => {
        return "org.asn1gen.runtime.java.AsnRelativeOidType"
      }
      case ast.SequenceOfType(_) => {
        return "org.asn1gen.runtime.java.AsnSequenceOf"
      }
      case ast.SequenceType(_) => {
        return "org.asn1gen.runtime.java.AsnSequence"
      }
      case ast.SetOfType(_) => {
        return "org.asn1gen.runtime.java.AsnSetOf"
      }
      case ast.SetType(_) => {
        return "org.asn1gen.runtime.java.AsnSet"
      }
      case ast.TaggedType(_, _, underlyingType) => {
        return rawDefaultOf(underlyingType)
      }
      case unmatched => {
        return "UnknownBuiltinType(" + unmatched + ")"
      }
    }
  }
  
  def rawDefaultOf(characterString: ast.CharacterStringType): String = {
    characterString match {
      case ast.BMPString => {
        return "\"\""
      }
      case ast.GeneralString => {
        return "\"\""
      }
      case ast.GraphicString => {
        return "\"\""
      }
      case ast.IA5String => {
        return "\"\""
      }
      case ast.ISO646String => {
        return "\"\""
      }
      case ast.NumericString => {
        return "\"\""
      }
      case ast.PrintableString => {
        return "\"\""
      }
      case ast.T61String => {
        return "\"\""
      }
      case ast.TeletexString => {
        return "\"\""
      }
      case ast.UniversalString => {
        return "\"\""
      }
      case ast.UTF8String => {
        return "\"\""
      }
      case ast.VideotexString => {
        return "\"\""
      }
      case ast.VisibleString => {
        return "\"\""
      }
      case unknown => {
        return "UnknownCharacterString(" + unknown + ")"
      }
    }
  }
  
  /////////
  
  def asnTypeOf(_type: ast.Type): String = {
    _type match {
      case ast.Type(typeKind, _) => asnTypeOf(typeKind)
    }
  }
  
  def asnTypeOf(typeKind: ast.TypeKind): String = {
    typeKind match {
      case builtinType: ast.BuiltinType => asnTypeOf(builtinType)
      case usefulType: ast.UsefulType => asnTypeOf(usefulType)
      case ast.TypeReference(reference) => reference
      case unmatched => "Unmatched(" + unmatched + ")"
    }
  }
  
  def asnTypeOf(typeKind: ast.TypeKind, value: ast.OptionalDefault[ast.Value]): String = {
    value match {
      case ast.Empty =>
        return asnTypeOf(typeKind)
      case ast.Default(value) =>
        return asnTypeOf(typeKind)
      case ast.Optional =>
        return "Option[" + asnTypeOf(typeKind) + "]"
    }
  }
  
  def asnTypeOf(builtinType: ast.UsefulType): String = {
    builtinType match {
      case ast.GeneralizedTime => {
        return "org.asn1gen.runtime.java.AsnGeneralizedTime"
      }
      case ast.ObjectDescriptor => {
        return "org.asn1gen.runtime.java.AsnObjectDescriptor"
      }
      case ast.UTCTime => {
        return "org.asn1gen.runtime.java.AsnUtcTime"
      }
      case unmatched => {
        return "UnknownUsefulType(" + unmatched + ")"
      }
    }
  }
  
  def asnTypeOf(builtinType: ast.BuiltinType): String = {
    builtinType match {
      case ast.BitStringType(_) => {
        return "org.asn1gen.runtime.java.AsnBitString"
      }
      case ast.BOOLEAN => {
        return "org.asn1gen.runtime.java.AsnBoolean"
      }
      case characterString: ast.CharacterStringType => {
        asnTypeOf(characterString)
      }
      case _: ast.ChoiceType => {
        return "org.asn1gen.runtime.java.AsnChoice"
      }
      case ast.EmbeddedPdvType => {
        return "org.asn1gen.runtime.java.AsnEmbeddedPdv"
      }
      case ast.EnumeratedType(_) => {
        return "org.asn1gen.runtime.java.AsnEnumeration"
      }
      case ast.EXTERNAL => {
        return "ExternalType"
      }
      case ast.InstanceOfType(_) => {
        return "InstanceOfType"
      }
      case ast.INTEGER(_) => {
        return "org.asn1gen.runtime.java.AsnInteger"
      }
      case ast.NULL => {
        return "org.asn1gen.runtime.java.AsnNull"
      }
      case _: ast.ObjectClassFieldType => {
        return "org.asn1gen.runtime.java.AsnObjectClassField"
      }
      case ast.ObjectIdentifierType => {
        return "org.asn1gen.runtime.java.AsnObjectIdentifier"
      }
      case ast.OctetStringType => {
        return "org.asn1gen.runtime.java.AsnOctetString"
      }
      case ast.REAL => {
        return "org.asn1gen.runtime.java.AsnReal"
      }
      case ast.RelativeOidType => {
        return "org.asn1gen.runtime.java.AsnRelativeOidType"
      }
      case ast.SequenceOfType(_) => {
        return "org.asn1gen.runtime.java.AsnSequenceOf"
      }
      case ast.SequenceType(_) => {
        return "org.asn1gen.runtime.java.AsnSequence"
      }
      case ast.SetOfType(_) => {
        return "org.asn1gen.runtime.java.AsnSetOf"
      }
      case ast.SetType(_) => {
        return "org.asn1gen.runtime.java.AsnSet"
      }
      case ast.TaggedType(_, _, underlyingType) => {
        return asnTypeOf(underlyingType)
      }
      case unmatched => {
        return "UnknownBuiltinType(" + unmatched + ")"
      }
    }
  }
  
  def asnTypeOf(characterString: ast.CharacterStringType): String = {
    characterString match {
      case ast.BMPString => {
        return "org.asn1gen.runtime.java.AsnBmpString"
      }
      case ast.GeneralString => {
        return "org.asn1gen.runtime.java.AsnGeneralString"
      }
      case ast.GraphicString => {
        return "org.asn1gen.runtime.java.AsnGraphicString"
      }
      case ast.IA5String => {
        return "org.asn1gen.runtime.java.AsnIa5String"
      }
      case ast.ISO646String => {
        return "org.asn1gen.runtime.java.AsnIso646String"
      }
      case ast.NumericString => {
        return "org.asn1gen.runtime.java.AsnNumericString"
      }
      case ast.PrintableString => {
        return "org.asn1gen.runtime.java.AsnPrintableString"
      }
      case ast.T61String => {
        return "org.asn1gen.runtime.java.AsnT61String"
      }
      case ast.TeletexString => {
        return "org.asn1gen.runtime.java.AsnTeletexString"
      }
      case ast.UniversalString => {
        return "org.asn1gen.runtime.java.AsnUniversalString"
      }
      case ast.UTF8String => {
        return "org.asn1gen.runtime.java.AsnUtf8String"
      }
      case ast.VideotexString => {
        return "org.asn1gen.runtime.java.AsnVideotexString"
      }
      case ast.VisibleString => {
        return "org.asn1gen.runtime.java.AsnVisibleString"
      }
      case unknown => {
        return "UnknownCharacterString(" + unknown + ")"
      }
    }
  }
  
  def safeId(id: String): String = {
    if (keywords contains id) {
      return "`" + id + "`"
    } else {
      return id.replaceAll("-", "_")
    }
  }
  
  def typeNameOf(namedComponentType: ast.NamedComponentType): String = {
    namedComponentType match {
      case ast.NamedComponentType(
        ast.NamedType(ast.Identifier(identifier), _type),
        value)
      => {
        typeNameOf(_type, value)
      }
    }
  }
  
  def typeNameOf(_type: ast.Type, value: ast.OptionalDefault[ast.Value]): String = {
    value match {
      case ast.Empty =>
        return typeNameOf(_type)
      case ast.Default(value) =>
        return typeNameOf(_type)
      case ast.Optional =>
        return "Option[" + typeNameOf(_type) + "]"
    }
  }
  
  def typeNameOf(_type: ast.Type): String = {
    _type match {
      case ast.Type(typeKind, _) => typeNameOf(typeKind)
    }
  }
  
  def typeNameOf(typeKind: ast.TypeKind): String = {
    typeKind match {
      case builtinType: ast.BuiltinType => typeNameOf(builtinType)
      case usefulType: ast.UsefulType => typeNameOf(usefulType)
      case ast.TypeReference(reference) => reference
      case unmatched => "Unmatched(" + unmatched + ")"
    }
  }
  
  def typeNameOf(typeKind: ast.TypeKind, value: ast.OptionalDefault[ast.Value]): String = {
    value match {
      case ast.Empty =>
        return typeNameOf(typeKind)
      case ast.Default(value) =>
        return typeNameOf(typeKind)
      case ast.Optional =>
        return "Option[" + typeNameOf(typeKind) + "]"
    }
  }
  
  def typeNameOf(usefulType: ast.UsefulType): String = {
    usefulType match {
      case ast.GeneralizedTime => {
        return "_meta_.AsnGeneralizedTime"
      }
      case ast.ObjectDescriptor => {
        return "_meta_.AsnObjectDescriptor"
      }
      case ast.UTCTime => {
        return "_meta_.AsnUtcTime"
      }
      case unmatched => {
        return "UnknownUsefulType(" + unmatched + ")"
      }
    }
  }
  
  def typeNameOf(builtinType: ast.BuiltinType): String = {
    builtinType match {
      case ast.BitStringType(_) => {
        return "_meta_.AsnBitString"
      }
      case ast.BOOLEAN => {
        return "_meta_.AsnBoolean"
      }
      case characterString: ast.CharacterStringType => {
        typeNameOf(characterString)
      }
      case _: ast.ChoiceType => {
        return "_meta_.AsnChoice"
      }
      case ast.EmbeddedPdvType => {
        return "_meta_.AsnEmbeddedPdv"
      }
      case ast.EnumeratedType(_) => {
        return "_meta_.AsnEnumeration"
      }
      case ast.EXTERNAL => {
        return "ExternalType"
      }
      case ast.InstanceOfType(_) => {
        return "InstanceOfType"
      }
      case ast.INTEGER(_) => {
        return "_meta_.AsnInteger"
      }
      case ast.NULL => {
        return "_meta_.AsnNull"
      }
      case _: ast.ObjectClassFieldType => {
        return "_meta_.AsnObjectClassField"
      }
      case ast.ObjectIdentifierType => {
        return "_meta_.AsnObjectIdentifier"
      }
      case ast.OctetStringType => {
        return "_meta_.AsnOctetString"
      }
      case ast.REAL => {
        return "_meta_.AsnReal"
      }
      case ast.RelativeOidType => {
        return "_meta_.AsnRelativeOidType"
      }
      case ast.SequenceOfType(_) => {
        return "_meta_.AsnSequenceOf"
      }
      case ast.SequenceType(_) => {
        return "_meta_.AsnSequence"
      }
      case ast.SetOfType(_) => {
        return "_meta_.AsnSetOf"
      }
      case ast.SetType(_) => {
        return "_meta_.AsnSet"
      }
      case ast.TaggedType(_, _, underlyingType) => {
        return typeNameOf(underlyingType)
      }
      case unmatched => {
        return "UnknownBuiltinType(" + unmatched + ")"
      }
    }
  }
  
  def typeNameOf(characterString: ast.CharacterStringType): String = {
    characterString match {
      case ast.BMPString => {
        return "_meta_.AsnBmpString"
      }
      case ast.GeneralString => {
        return "_meta_.AsnGeneralString"
      }
      case ast.GraphicString => {
        return "_meta_.AsnGraphicString"
      }
      case ast.IA5String => {
        return "_meta_.AsnIa5String"
      }
      case ast.ISO646String => {
        return "_meta_.AsnIso646String"
      }
      case ast.NumericString => {
        return "_meta_.AsnNumericString"
      }
      case ast.PrintableString => {
        return "_meta_.AsnPrintableString"
      }
      case ast.T61String => {
        return "_meta_.AsnT61String"
      }
      case ast.TeletexString => {
        return "_meta_.AsnTeletexString"
      }
      case ast.UniversalString => {
        return "_meta_.AsnUniversalString"
      }
      case ast.UTF8String => {
        return "_meta_.AsnUtf8String"
      }
      case ast.VideotexString => {
        return "_meta_.AsnVideotexString"
      }
      case ast.VisibleString => {
        return "_meta_.AsnVisibleString"
      }
      case unknown => {
        return "UnknownCharacterString(" + unknown + ")"
      }
    }
  }
}
