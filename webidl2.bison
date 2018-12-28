%%

Definitions
  : ExtendedAttributeList Definition Definitions
  | ε
  ;

Definition
  : CallbackOrInterfaceOrMixin
  | Namespace
  | Partial
  | Dictionary
  | Enum
  | Typedef
  | IncludesStatement
  ;
ArgumentNameKeyword
  : attribute
  | callback
  | const
  | deleter
  | dictionary
  | enum
  | getter
  | includes
  | inherit
  | interface
  | iterable
  | maplike
  | namespace
  | partial
  | required
  | setlike
  | setter
  | static
  | stringifier
  | typedef
  | unrestricted
  ;
CallbackOrInterfaceOrMixin
  : callback CallbackRestOrInterface
  | interface InterfaceOrMixin
  ;
CallbackRestOrInterface
  : CallbackRest
  | interface InterfaceRest
  ;
InterfaceOrMixin
  : InterfaceRest
  | MixinRest
  ;
InterfaceRest
  : IDENTIFIER Inheritance '{' InterfaceMembers '}' ';'
  ;
Partial
  : partial PartialDefinition
  ;
PartialDefinition
  : interface PartialInterfaceOrPartialMixin
  | PartialDictionary
  | Namespace
  ;
PartialInterfaceOrPartialMixin
  : PartialInterfaceRest
  | MixinRest
  ;
PartialInterfaceRest
  : IDENTIFIER '{' InterfaceMembers '}' ';'
  ;
InterfaceMembers
  : ExtendedAttributeList InterfaceMember InterfaceMembers
  | ε
  ;
InterfaceMember
  : Const
  | Operation
  | Stringifier
  | StaticMember
  | Iterable
  | ReadOnlyMember
  | ReadWriteAttribute
  | ReadWriteMaplike
  | ReadWriteSetlike
  ;
Inheritance
  : ':' IDENTIFIER
  | ε
  ;
MixinRest
  : mixin IDENTIFIER '{' MixinMembers '}' ';'
  ;
MixinMembers
  : ExtendedAttributeList MixinMember MixinMembers
  | ε
  ;
MixinMember
  : Const
  | RegularOperation
  | Stringifier
  | ReadOnly AttributeRest
  ;
IncludesStatement
  : IDENTIFIER includes IDENTIFIER ';'
  ;
Const
  : const ConstType IDENTIFIER '=' ConstValue ';'
  ;
ConstValue
  : BooleanLiteral
  | FloatLiteral
  | INTEGER
  | null
  ;
BooleanLiteral
  : true
  | false
  ;
FloatLiteral
  : FLOAT
  | "-Infinity"
  | "Infinity"
  | "NaN"
  ;
ConstType
  : PrimitiveType Null
  | IDENTIFIER Null
  ;
ReadOnlyMember
  : readonly ReadOnlyMemberRest
  ;
ReadOnlyMemberRest
  : AttributeRest
  | ReadWriteMaplike
  | ReadWriteSetlike
  ;
ReadWriteAttribute
  : inherit ReadOnly AttributeRest
  | AttributeRest
  ;
AttributeRest
  : attribute TypeWithExtendedAttributes AttributeName ';'
  ;
AttributeName
  : AttributeNameKeyword
  | IDENTIFIER
  ;
AttributeNameKeyword
  : required
  ;
ReadOnly
  : readonly
  | ε
  ;
DefaultValue
  : ConstValue
  | STRING
  | '[' ']'
  ;
Operation
  : RegularOperation
  | SpecialOperation
  ;
RegularOperation
  : ReturnType OperationRest
  ;
SpecialOperation
  : Special RegularOperation
  ;
Special
  : getter
  | setter
  | deleter
  ;
OperationRest
  : OptionalIdentifier '(' ArgumentList ')' ';'
  ;
OptionalIdentifier
  : IDENTIFIER
  | ε
  ;
ArgumentList
  : Argument Arguments
  | ε
  ;
Arguments
  : ',' Argument Arguments
  | ε
  ;
Argument
  : ExtendedAttributeList ArgumentRest
  ;
ArgumentRest
  : optional TypeWithExtendedAttributes ArgumentName Default
  | Type Ellipsis ArgumentName
  ;
ArgumentName
  : ArgumentNameKeyword
  | IDENTIFIER
  ;
Ellipsis
  : "..."
  | ε
  ;
ReturnType
  : Type
  | void
  ;
Stringifier
  : stringifier StringifierRest
  ;
StringifierRest
  : ReadOnly AttributeRest
  | RegularOperation
  | ';'
  ;
StaticMember
  : static StaticMemberRest
  ;
StaticMemberRest
  : ReadOnly AttributeRest
  | RegularOperation
  ;
Iterable
  : iterable '<' TypeWithExtendedAttributes OptionalType '>' ';'
  ;
OptionalType
  : ',' TypeWithExtendedAttributes
  | ε
  ;
ReadWriteMaplike
  : MaplikeRest
  ;
MaplikeRest
  : maplike '<' TypeWithExtendedAttributes ',' TypeWithExtendedAttributes '>' ';'
  ;
ReadWriteSetlike
  : SetlikeRest
  ;
SetlikeRest
  : setlike '<' TypeWithExtendedAttributes '>' ';'
  ;
Namespace
  : namespace IDENTIFIER '{' NamespaceMembers '}' ';'
  ;
NamespaceMembers
  : ExtendedAttributeList NamespaceMember NamespaceMembers
  | ε
  ;
NamespaceMember
  : RegularOperation
  | readonly AttributeRest
  ;
Dictionary
  : dictionary IDENTIFIER Inheritance '{' DictionaryMembers '}' ';'
  ;
DictionaryMembers
  : DictionaryMember DictionaryMembers
  | ε
  ;
DictionaryMember
  : ExtendedAttributeList DictionaryMemberRest
  ;
DictionaryMemberRest
  : required TypeWithExtendedAttributes IDENTIFIER Default ';'
  | Type IDENTIFIER Default ';'
  ;
PartialDictionary
  : dictionary IDENTIFIER '{' DictionaryMembers '}' ';'
  ;
Default
  : '=' DefaultValue
  | ε
  ;
Enum
  : enum IDENTIFIER '{' EnumValueList '}' ';'
  ;
EnumValueList
  : STRING EnumValueListComma
  ;
EnumValueListComma
  : ',' EnumValueListString
  | ε
  ;
EnumValueListString
  : STRING EnumValueListComma
  | ε
  ;
CallbackRest
  : IDENTIFIER '=' ReturnType '(' ArgumentList ')' ';'
  ;
Typedef
  : typedef TypeWithExtendedAttributes IDENTIFIER ';'
  ;
Type
  : SingleType
  | UnionType Null
  ;
TypeWithExtendedAttributes
  : ExtendedAttributeList Type
  ;
SingleType
  : NonAnyType
  | any
  ;
UnionType
  : '(' UnionMemberType or UnionMemberType UnionMemberTypes ')'
  ;
UnionMemberType
  : ExtendedAttributeList NonAnyType
  | UnionType Null
  ;
UnionMemberTypes
  : or UnionMemberType UnionMemberTypes
  | ε
  ;
NonAnyType
  : PromiseType ε
  | PrimitiveType Null
  | StringType Null
  | IDENTIFIER Null
  | sequence '<' TypeWithExtendedAttributes '>' Null
  | object Null
  | symbol Null
  | Error Null
  | BufferRelatedType Null
  | FrozenArray '<' TypeWithExtendedAttributes '>' Null
  | RecordType Null
  ;
PrimitiveType
  : UnsignedIntegerType
  | UnrestrictedFloatType
  | boolean
  | byte
  | octet
  ;
UnrestrictedFloatType
  : unrestricted FloatType
  | FloatType
  ;
FloatType
  : FLOAT
  | double
  ;
UnsignedIntegerType
  : unsigned IntegerType
  | IntegerType
  ;
IntegerType
  : short
  | long OptionalLong
  ;
OptionalLong
  : long
  | ε
  ;
StringType
  : "ByteString"
  | "DOMString"
  | "USVString"
  ;
PromiseType
  : Promise '<' ReturnType '>'
  ;
RecordType
  : record '<' StringType ',' TypeWithExtendedAttributes '>'
  ;
Null
  : '?'
  | ε
  ;
BufferRelatedType
  : ArrayBuffer
  | DataView
  | Int8Array
  | Int16Array
  | Int32Array
  | Uint8Array
  | Uint16Array
  | Uint32Array
  | Uint8ClampedArray
  | Float32Array
  | Float64Array
  ;
ExtendedAttributeList
  : '[' ExtendedAttribute ExtendedAttributes ']'
  | ε
  ;
ExtendedAttributes
  : ',' ExtendedAttribute ExtendedAttributes
  | ε
  ;
ExtendedAttribute
  : '(' ExtendedAttributeInner ')' ExtendedAttributeRest
  | '[' ExtendedAttributeInner ']' ExtendedAttributeRest
  | '{' ExtendedAttributeInner '}' ExtendedAttributeRest
  | Other ExtendedAttributeRest
  ;
ExtendedAttributeRest
  : ExtendedAttribute
  | ε
  ;
ExtendedAttributeInner
  : '(' ExtendedAttributeInner ')' ExtendedAttributeInner
  | '[' ExtendedAttributeInner ']' ExtendedAttributeInner
  | '{' ExtendedAttributeInner '}' ExtendedAttributeInner
  | OtherOrComma ExtendedAttributeInner
  | ε
  ;
Other
  : INTEGER
  | FLOAT
  | IDENTIFIER
  | STRING
  | OTHER
  | '-'
  | "-Infinity"
  | '.'
  | "..."
  | ':'
  | ';'
  | '<'
  | '='
  | '>'
  | '?'
  | "ByteString"
  | "DOMString"
  | FrozenArray
  | "Infinity"
  | "NaN"
  | "USVString"
  | any
  | boolean
  | byte
  | double
  | false
  | FLOAT
  | long
  | null
  | object
  | octet
  | or
  | optional
  | sequence
  | short
  | true
  | unsigned
  | void
  | ArgumentNameKeyword
  | BufferRelatedType
  ;
OtherOrComma
  : Other
  | ','
  ;
IdentifierList
  : IDENTIFIER Identifiers
  ;
Identifiers
  : ',' IDENTIFIER Identifiers
  | ε
  ;
ExtendedAttributeNoArgs
  : IDENTIFIER
  ;
ExtendedAttributeArgList
  : IDENTIFIER '(' ArgumentList ')'
  ;
ExtendedAttributeIdent
  : IDENTIFIER '=' IDENTIFIER
  ;
ExtendedAttributeIdentList
  : IDENTIFIER '=' '(' IdentifierList ')'
  ;
ExtendedAttributeNamedArgList
  : IDENTIFIER '=' IDENTIFIER '(' ArgumentList ')'
  ;
