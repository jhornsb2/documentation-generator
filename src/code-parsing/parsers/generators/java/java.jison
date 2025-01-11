%{
    // TODO: Define the tokens that will be used in the grammar
%}

%%

// ============================================================================
// Compilation Units
// ============================================================================
/**
 * Represents a Java compilation unit. Usually a .java file.
 */
CompilationUnit
    : OrdinaryCompilationUnit
    | ModularCompilationUnit
    ;

/**
 * The standard Java file structure.
 */
OrdinaryCompilationUnit
    : OptionalPackageDeclaration OptionalImportDeclarations TopLevelClassOrInterfaceDeclarations
    ;

/**
 * The Java file structure for modules.
 */
ModularCompilationUnit
    : OptionalImportDeclarations ModuleDeclaration
    ;

// ============================================================================
// Module Declarations
// ============================================================================
ModuleDeclaration
    : Annotations OptionalOpen MODULE ModuleDeclarationModuleName LBRACE ModuleDirectiveList RBRACE
    ;

OptionalOpen
    : /* empty */
    | OPEN
    ;

ModuleDeclarationModuleName
    : Identifier
    | PackageDeclarationPackageName DOT Identifier
    ;

ModuleDirectiveList
    : /* empty */
    | ModuleDirective ModuleDirectiveList
    ;

ModuleDirective
    : ModuleRequiresDirective
    | ModuleExportsDirective
    | ModuleOpensDirective
    | ModuleUsesDirective
    | ModuleProvidesDirective
    ;

ModuleRequiresDirective
    : REQUIRES RequiresModifiers ModuleName SEMICOLON
    ;

ModuleExportsDirective
    : EXPORTS PackageName SEMICOLON
    | EXPORTS PackageName TO ModuleNameList SEMICOLON
    ;

ModuleOpensDirective
    : OPENS PackageName SEMICOLON
    | OPENS PackageName TO ModuleNameList SEMICOLON
    ;

ModuleUsesDirective
    : USES TypeName SEMICOLON
    ;

ModuleProvidesDirective
    : PROVIDES TypeName WITH TypeNameList SEMICOLON
    ;

// ============================================================================
// Package Declarations
// ============================================================================
OptionalPackageDeclaration
    : /* empty */
    | PackageDeclaration
    ;

PackageDeclaration
    : PackageModifiers PACKAGE PackageDeclarationPackageName SEMICOLON
    ;

PackageDeclarationPackageName
    : Identifier
    | PackageDeclarationPackageName DOT Identifier
    ;

// ============================================================================
// Import Declarations
// ============================================================================
OptionalImportDeclarations
    : /* empty */
    | ImportDeclarations
    ;

ImportDeclarations
    : ImportDeclaration
    | ImportDeclarations ImportDeclaration
    ;

ImportDeclaration
    : SingleTypeImportDeclaration
    | TypeImportOnDemandDeclaration
    | SingleStaticImportDeclaration
    | StaticImportOnDemandDeclaration
    ;

SingleTypeImportDeclaration
    : IMPORT TypeName SEMICOLON
    ;

TypeImportOnDemandDeclaration
    : IMPORT PackageOrTypeName DOT Splat SEMICOLON
    ;

SingleStaticImportDeclaration
    : IMPORT STATIC TypeName DOT Identifier SEMICOLON
    ;

StaticImportOnDemandDeclaration
    : IMPORT STATIC TypeName DOT Splat SEMICOLON
    ;

Splat // Helper rule for on demand imports. The * is used for this AND for multiplication, so we need to differentiate.
    : TIMES
    ;

// ============================================================================
// Top Level Declarations
// ============================================================================
TopLevelClassOrInterfaceDeclarations
    : /* empty */
    | TopLevelClassOrInterfaceDeclaration TopLevelClassOrInterfaceDeclarations
    ;

TopLevelClassOrInterfaceDeclaration
    : ClassDeclaration
    | InterfaceDeclaration
    ;

// ============================================================================
// Class/Interface Declarations
// ============================================================================
// Class declaration
ClassDeclaration
    : NormalClassDeclaration
    | EnumDeclaration
    | RecordDeclaration
    ;

// Normal Class Declaration
NormalClassDeclaration
    : ClassModifiers CLASS TypeIdentifier OptionalTypeParameters ClassExtends OptionalClassImplements ClassPermits ClassBody
    ;

// Enum declaration
EnumDeclaration
    : ClassModifiers ENUM TypeIdentifier OptionalClassImplements EnumBody
    ;

// Record declaration
RecordDeclaration
    : ClassModifiers RECORD TypeIdentifier OptionalTypeParameters RecordHeader OptionalClassImplements RecordBody
    ;

// Interface declaration
InterfaceDeclaration
    : NormalInterfaceDeclaration
    | AnnotationInterfaceDeclaration
    ;

// Normal interface declaration
NormalInterfaceDeclaration
    : InterfaceModifiers INTERFACE TypeIdentifier OptionalTypeParameters OptionalInterfaceExtends InterfacePermits InterfaceBody
    ;

// Annotation interface declaration
AnnotationInterfaceDeclaration
    : InterfaceModifiers AT INTERFACE TypeIdentifier AnnotationInterfaceBody
    ;

// ============================================================================
// Modifiers
// ============================================================================
// Requires modifiers
RequiresModifiers
    : /* empty */
    | RequiresModifier RequiresModifiers
    ;

RequiresModifier
    : TRANSITIVE
    | STATIC
    ;

// Package modifiers
PackageModifiers
    : /* empty */
    | PackageModifier PackageModifiers
    ;

PackageModifier
    : Annotation
    ;

// Class modifiers
ClassModifiers
    : /* empty */
    | ClassModifier ClassModifiers
    ;

ClassModifier
    : Annotation
    | PUBLIC
    | PROTECTED
    | PRIVATE
    | ABSTRACT
    | STATIC
    | FINAL
    | SEALED
    | NON_SEALED
    | STRICTFP
    ;

// Interface modifiers
InterfaceModifiers
    : /* empty */
    | InterfaceModifier InterfaceModifiers
    ;

InterfaceModifier
    : Annotation
    | PUBLIC
    | PROTECTED
    | PRIVATE
    | ABSTRACT
    | STATIC
    | SEALED
    | NON_SEALED
    | STRICTFP
    ;

// Member modifiers
// Annotation interface element modifiers
AnnotationInterfaceElementModifiers
    : /* empty */
    | AnnotationInterfaceElementModifier AnnotationInterfaceElementModifiers
    ;

AnnotationInterfaceElementModifier
    : Annotation
    | PUBLIC
    | ABSTRACT
    ;

// Enum constant modifiers
EnumConstantModifiers
    : /* empty */
    | EnumConstantModifier EnumConstantModifiers
    ;

EnumConstantModifier
    : Annotation
    ;

// Constant modifiers
ConstantModifiers
    : /* empty */
    | ConstantModifier ConstantModifiers
    ;

ConstantModifier
    : Annotation
    | PUBLIC
    | STATIC
    | FINAL
    ;

// Constructor modifiers
ConstructorModifiers
    : /* empty */
    | ConstructorModifier ConstructorModifiers
    ;

ConstructorModifier
    : Annotation
    | PUBLIC
    | PROTECTED
    | PRIVATE
    ;

// Method Modifiers
MethodModifiers
    : /* empty */
    | MethodModifier MethodModifiers
    ;

MethodModifier
    : Annotation
    | PUBLIC
    | PROTECTED
    | PRIVATE
    | ABSTRACT
    | STATIC
    | FINAL
    | SYNCHRONIZED
    | NATIVE
    | STRICTFP
    ;

// Field Modifiers
FieldModifiers
    : /* empty */
    | FieldModifier FieldModifiers
    ;

FieldModifier
    : Annotation
    | PUBLIC
    | PROTECTED
    | PRIVATE
    | STATIC
    | FINAL
    | TRANSIENT
    | VOLATILE
    ;

// Record component modifiers
RecordComponentModifiers:
    : /* empty */
    | RecordComponentModifier RecordComponentModifiers
    ;

RecordComponentModifier
    : Annotation
    ;

// Interface method modifiers
InterfaceMethodModifiers
    : /* empty */
    | InterfaceMethodModifier InterfaceMethodModifiers
    ;

InterfaceMethodModifier
    : Annotation
    | PUBLIC
    | PRIVATE
    | ABSTRACT
    | DEFAULT
    | STATIC
    | STRICTFP
    ;

// Variable modifiers
VariableModifierList
    : /* empty */
    | VariableModifier
    | VariableModifier VariableModifierList
    ;

VariableModifier
    : ANNOTATIONS
    | FINAL
    ;

// Type parameter modifiers
TypeParameterModifiers
    : /* empty */
    | TypeParameterModifier TypeParameterModifiers
    ;

TypeParameterModifier
    : Annotation
    ;

// ============================================================================
// Bodies
// ============================================================================
// Class bodies
// Normal class body
OptionalClassBody
    : /* empty */
    | ClassBody
    ;

ClassBody
    : LBRACE ClassBodyDeclarations RBRACE
    ;

// Enum class body
EnumBody
    : LBRACE OptionalEnumConstantList OptionalComma EnumBodyDeclarations RBRACE
    ;

// Record class body
RecordBody
    : LBRACE RecordBodyDeclarations RBRACE
    ;

// Interface bodies
// Normal interface body
InterfaceBody
    : LBRACE InterfaceMemberDeclarations RBRACE
    ;

// Annotation interface body
AnnotationInterfaceBody
    : LBRACE AnnotationInterfaceMemberDeclarations RBRACE
    ;


// Constructor body
ConstructorBody
    : LBRACE OptionalExplicitConstructorInvocation BlockStatements RBRACE
    ;

// Method body
MethodBody
    : Block
    | SEMICOLON
    ;

// ============================================================================
// Body Member declarations
// ============================================================================
// Class body declarations
ClassBodyDeclarations
    : /* empty */
    | ClassBodyDeclaration ClassBodyDeclarations
    ;

ClassBodyDeclaration
    : ClassMemberDeclaration
    | InstanceInitializer
    | StaticInitializer
    | ConstructorDeclaration
    ;

// Enum body member declarations
EnumBodyDeclarations
    : /* empty */
    | SEMICOLON ClassBodyDeclarations
    ;

// Enum constants
OptionalEnumConstantList
    : /* empty */
    | EnumConstantList
    ;

EnumConstantList
    : EnumConstant
    | EnumConstant COMMA EnumConstantList
    ;

EnumConstant
    : EnumConstantModifiers Identifier OptionalParenthesizedArguments OptionalClassBody
    ;

// Record body declarations
RecordBodyDeclarations
    : /* empty */
    | RecordBodyDeclaration RecordBodyDeclarations
    ;

RecordBodyDeclaration
    : ClassBodyDeclaration
    | CompactConstructorDeclaration
    ;

// Interface body member declarations
InterfaceMemberDeclarations
    : /* empty */
    | InterfaceMemberDeclaration InterfaceMemberDeclarations
    ;

InterfaceMemberDeclaration
    : ConstantDeclaration
    | InterfaceMethodDeclaration
    | ClassDeclaration
    | InterfaceDeclaration
    | SEMICOLON
    ;

// Annotation interface member declarations
AnnotationInterfaceMemberDeclarations
    : /* empty */
    | AnnotationInterfaceMemberDeclaration AnnotationInterfaceMemberDeclarations
    ;

AnnotationInterfaceMemberDeclaration
    : AnnotationInterfaceElementDeclaration
    | ConstantDeclaration
    | ClassDeclaration
    | InterfaceDeclaration
    | SEMICOLON
    ;

// ============================================================================
// Class/Interface member declarations
// ============================================================================
// Class member declaration
ClassMemberDeclaration
    : FieldDeclaration
    | MethodDeclaration
    | ClassDeclaration
    | InterfaceDeclaration
    | SEMICOLON
    ;
// Constructor declaration
ConstructorDeclaration
    : ConstructorModifiers ConstructorDeclarator OptionalThrows ConstructorBody
    ;

ConstructorDeclarator
    : OptionalTypeParameters SimpleTypeName LPAREN OptionalReceiverParameterComma OptionalFormalParameterList RPAREN
    ;

// Compact constructor declaration
CompactConstructorDeclaration
    : ConstructorModifiers SimpleTypeName ConstructorBody
    ;

// Field declaration
FieldDeclaration
    : FieldModifiers UnannType VariableDeclaratorList SEMICOLON
    ;

// Method declaration
MethodDeclaration
    : MethodModifiers MethodHeader MethodBody
    ;

// Constant declaration
ConstantDeclaration
    : ConstantModifiers UnannType VariableDeclaratorList SEMICOLON
    ;

// Interface method declaration
InterfaceMethodDeclaration
    : InterfaceMethodModifiers MethodHeader MethodBody
    ;

// Annotation interface element declaration
AnnotationInterfaceElementDeclaration
    : AnnotationInterfaceElementModifiers UnannType Identifier LPAREN RPAREN OptionalDims OptionalDefaultValue SEMICOLON
    ;

// Annotation interface element declaration default value
OptionalDefaultValue
    : /* empty */
    | DefaultValue
    ;

DefaultValue
    : DEFAULT ElementValue
    ;

// ============================================================================
// Headers
// ============================================================================
// Record Header
RecordHeader
    : LPAREN OptionalRecordComponentList RPAREN
    ;

OptionalRecordComponentList
    : /* empty */
    | RecordComponentList
    ;

RecordComponentList
    : RecordComponent
    | RecordComponent COMMA RecordComponentList
    ;

RecordComponent
    : RecordComponentModifiers UnannType Identifier
    | VariableArityRecordComponent
    ;

VariableArityRecordComponent
    : RecordComponentModifiers UnannType Annotations DOT DOT DOT Identifier
    ;

// Method header
MethodHeader
    : Result MethodDeclarator OptionalThrows
    | TypeParameters Annotations Result MethodDeclarator OptionalThrows
    ;

Result
    : UnannType
    | VOID
    ;

MethodDeclarator
    : Identifier LPAREN OptionalReceiverParameterComma OptionalFormalParameterList RPAREN OptionalDims
    ;

// ============================================================================
// Initializers
// ============================================================================
InstanceInitializer
    : Block
    ;

StaticInitializer
    : STATIC Block
    ;

// ============================================================================
// Extends/Implements/Permits
// ============================================================================
// Interface extends
OptionalInterfaceExtends
    : /* empty */
    | InterfaceExtends
    ;

InterfaceExtends
    : EXTENDS InterfaceTypeList
    ;

// Interface permits
OptionalInterfacePermits
    : /* empty */
    | InterfacePermits
    ;

InterfacePermits
    : PERMITS TypeNameList
    ;


// Class extends
ClassExtends
    : /* empty */
    | EXTENDS ClassType
    ;
// Class implements
OptionalClassImplements
    : /* empty */
    | ClassImplements
    ;

ClassImplements
    : IMPLEMENTS InterfaceTypeList
    ;

InterfaceTypeList
    : InterfaceType
    | InterfaceType COMMA InterfaceTypeList
    ;

// Class permits
ClassPermits
    : /* empty */
    | PERMITS TypeNameList
    ;

// ============================================================================
// Throws
// ============================================================================
OptionalThrows
    : /* empty */
    | Throws
    ;

Throws
    : THROWS ExceptionTypeList
    ;

ExceptionTypeList
    : ExceptionType
    | ExceptionType COMMA ExceptionTypeList
    ;

ExceptionType
    : ClassType
    | TypeVariable
    ;

// ============================================================================
// Types
// ============================================================================
Type
    : PrimitiveType
    | ReferenceType
    ;

// Primitive type
PrimitiveType
    : Annotations NumericType
    | Annotations BOOLEAN
    ;

// Reference type
ReferenceType
    : ClassOrInterfaceType
    | TypeVariable
    | ArrayType
    ;

// Local Variable Type
LocalVariableType
    : UnannType
    | VAR
    ;

// Lambda parameter type
LambdaParameterType
    : UnannType
    | VAR
    ;

// Catch type
CatchType
    : UnannClassType OptionalUnionedClassTypeList
    ;

OptionalUnionedClassTypeList
    : /* empty */
    | UnionedClassTypeList
    ;

UnionedClassTypeList
    : BITWISE_OR ClassType
    | UnionedClassTypeList BITWISE_OR ClassType
    ;

// ============================================================================
// Numeric Types
// ============================================================================
NumericType
    : IntegralType
    | FloatingPointType
    ;

// Integral type
IntegralType
    : BYTE
    | SHORT
    | INT
    | LONG
    | CHAR
    ;

// Floating point type
FloatingPointType
    : FLOAT
    | DOUBLE
    ;

// ============================================================================
// Reference Types
// ============================================================================
// Class or interface type
ClassOrInterfaceType
    : ClassType
    | InterfaceType
    ;

// Type variable
TypeVariable
    : Annotations TypeIdentifier
    ;

// Array type
ArrayType
    : PrimitiveType Dims
    | ClassOrInterfaceType Dims
    | TypeVariable Dims
    ;

// ============================================================================
// Class or Interface Types
// ============================================================================
// Class type
ClassType
    : Annotations TypeIdentifier OptionalTypeArguments
    | PackageName DOT Annotations TypeIdentifier OptionalTypeArguments
    | ClassOrInterfaceType DOT Annotations TypeIdentifier OptionalTypeArguments
    ;

// Interface type
InterfaceType
    : ClassType
    ;

// ============================================================================
// Unannotated Types
// ============================================================================
UnannType
    : UnannPrimitiveType
    | UnannReferenceType
    ;

UnannPrimitiveType
    : NumericType
    | BOOLEAN
    ;

UnannReferenceType
    : UnannClassOrInterfaceType
    | UnannTypeVariable
    | UnannArrayType
    ;

UnannClassOrInterfaceType
    : UnannClassType
    | UnannInterfaceType
    ;

UnannInterfaceType
    : UnannClassType
    ;

UnannClassType
    : TypeIdentifier OptionalTypeArguments
    | PackageName DOT Annotations TypeIdentifier OptionalTypeArguments
    | UnannClassOrInterfaceType DOT Annotations TypeIdentifier OptionalTypeArguments
    ;

UnannTypeVariable
    : TypeIdentifier
    ;

UnannArrayType
    : UnannPrimitiveType Dims
    | UnannClassOrInterfaceType Dims
    | UnannTypeVariable Dims
    ;

// ============================================================================
// Annotations
// ============================================================================
Annotations
    : /* empty */
    | Annotation Annotations
    ;

// Annotation
Annotation
    : NormalAnnotation
    | MarkerAnnotation
    | SingleElementAnnotation
    ;

// Normal annotation
NormalAnnotation
    : AT TypeName LPAREN ElementValuePairList RPAREN
    ;

// Marker annotation
MarkerAnnotation
    : AT TypeName
    ;

// Single element annotation
SingleElementAnnotation
    : AT TypeName LPAREN ElementValue RPAREN
    ;

// ============================================================================
// Element Values
// ============================================================================
// Element value pair list
ElementValuePairList
    : ElementValuePair
    | ElementValuePair COMMA ElementValuePairList
    ;

// Element value pair
ElementValuePair
    : Identifier ASSIGN ElementValue
    ;

// Element value array initializer
ElementValueArrayInitializer
    : LBRACE ElementValueList COMMA RBRACE
    | LBRACE ElementValueList RBRACE
    | LBRACE COMMA RBRACE
    | LBRACE RBRACE
    ;

ElementValueList
    : ElementValue
    | ElementValue COMMA ElementValueList
    ;

// Element value
ElementValue
    : ConditionalExpression
    | ElementValueArrayInitializer
    | Annotation
    ;

// ============================================================================
// Names and Identifiers
// ============================================================================
// Module name
ModuleNameList
    : ModuleName
    | ModuleName COMMA ModuleNameList
    ;
ModuleName
    : Identifier
    | ModuleName DOT Identifier
    ;
// Package or type name
PackageOrTypeName
    : Identifier
    | PackageOrTypeName DOT Identifier
    ;

// Package Name
PackageName
    : Identifier
    | PackageName DOT Identifier
    ;

// Type names
TypeNameList
    : TypeName
    | TypeName COMMA TypeNameList
    ;

TypeName
    : PackageOrTypeName DOT TypeIdentifier
    | TypeIdentifier
    ;

SimpleTypeName
    : TypeIdentifier
    ;

// Expression Name
ExpressionName
    : Identifier
    | AmbiguousName DOT Identifier
    ;

// Ambiguous Name
AmbiguousName
    : Identifier
    | AmbiguousName DOT Identifier
    ;
// Type Identifier
TypeIdentifier // https://docs.oracle.com/javase/specs/jls/se23/html/jls-3.html#jls-TypeIdentifier
    : Identifier
    ;
// Method Name
MethodName:
    : UnqualifiedMethodIdentifier
    ;

UnqualifiedMethodIdentifier
    : Identifier
    ;

// ============================================================================
// Arguments and Parameters
// ============================================================================
OptionalParenthesizedArguments
    : /* empty */
    | LPAREN OptionalArgumentList RPAREN
    ;

OptionalArgumentList
    : /* empty */
    | ArgumentList
    ;

ArgumentList
    : Expression
    : Expression COMMA ArgumentList
    ;

// Type Arguments
OptionalTypeArgumentsOrDiamond
    : /* empty */
    | TypeArgumentsOrDiamond
    ;

TypeArgumentsOrDiamond
    : LT GT
    | TypeArguments
    ;

OptionalTypeArguments
    : /* empty */
    | TypeArguments
    ;

TypeArguments
    : LT TypeArgumentList GT
    ;

TypeArgumentList
    : TypeArgument
    | TypeArgument COMMA TypeArgumentList
    ;

TypeArgument
    : ReferenceType
    | Wildcard
    ;

Wildcard
    : Annotations QUESTION_MARK WildcardBounds
    ;

// Type parameters
OptionalTypeParameters
    : /* empty */
    | TypeParameters
    ;

TypeParameters
    : LT TypeParameterList GT
    ;

TypeParameterList
    : TypeParameter
    | TypeParameter COMMA TypeParameterList
    ;

TypeParameter
    : TypeParameterModifiers TypeIdentifier OptionalTypeBound
    ;

// Receiver Parameter
OptionalReceiverParameterComma
    : /* empty */
    | ReceiverParameter COMMA
    ;

ReceiverParameter
    : Annotations UnannType Identifier DOT THIS
    | Annotations UnannType THIS
    ;

// Formal parameters
OptionalFormalParameterList
    : /* empty */
    | FormalParameterList
    ;

FormalParameterList
    : FormalParameter
    | FormalParameter COMMA FormalParameterList
    ;

FormalParameter
    : VariableModifierList UnannType VariableDeclaratorId
    | VariableArityParameter
    ;

VariableArityParameter
    : VariableModifierList UnannType Annotations DOT DOT DOT Identifier
    ;

// ============================================================================
// Bounds
// ============================================================================
// Type bound
OptionalTypeBound
    : /* empty */
    | TypeBound
    ;

TypeBound
    : EXTENDS TypeVariable
    | EXTENDS ClassOrInterfaceType AdditionalBoundList
    ;

WildcardBounds
    : /* empty */
    | EXTENDS ReferenceType
    | SUPER ReferenceType
    ;

// Additional bounds
AdditionalBoundList
    : /* empty */
    | AdditionalBound AdditionalBoundList
    ;

AdditionalBound
    : BITWISE_AND InterfaceType
    ;

// ============================================================================
// Block
// ============================================================================
Block
    : LBRACE BlockStatements RBRACE
    ;

// ============================================================================
// Dims
// ============================================================================
OptionalDims
    : /* empty */
    | Dims
    ;

Dims
    : Annotations LBRACKET RBRACKET
    | Annotations LBRACKET RBRACKET Dims
    ;

// ============================================================================
// Statements
// ============================================================================
// Explicit constructor invocation
OptionalExplicitConstructorInvocation
    : /* empty */
    | ExplicitConstructorInvocation
    ;

ExplicitConstructorInvocation
    : OptionalTypeArguments THIS LPAREN OptionalArgumentList RPAREN SEMICOLON
    | OptionalTypeArguments SUPER LPAREN OptionalArgumentList RPAREN SEMICOLON
    | ExpressionName DOT OptionalTypeArguments SUPER LPAREN OptionalArgumentList RPAREN SEMICOLON
    | Primary DOT OptionalTypeArguments SUPER LPAREN OptionalArgumentList RPAREN SEMICOLON
    ;

// Block Statements
BlockStatements
    : /* empty */
    | BlockStatement BlockStatements
    ;

BlockStatement
    : LocalClassOrInterfaceDeclaration
    | LocalVariableDeclarationStatement
    | Statement
    ;

// Local class or interface declaration
LocalClassOrInterfaceDeclaration
    : ClassDeclaration
    | NormalInterfaceDeclaration
    ;

// Local Variable Declaration
LocalVariableDeclarationStatement
    : LocalVariableDeclaration SEMICOLON
    ;

// Statement
Statement // https://docs.oracle.com/javase/specs/jls/se23/html/jls-14.html#jls-Statement
    : StatementWithoutTrailingSubstatement
    | LabeledStatement
    | IfThenStatement
    | IfThenElseStatement
    | WhileStatement
    | ForStatement
    ;

StatementNoShortIf
    : StatementWithoutTrailingSubstatement
    | LabeledStatementNoShortIf
    | IfThenElseStatementNoShortIf
    | WhileStatementNoShortIf
    | ForStatementNoShortIf
    ;

// Statement without trailing substatement
StatementWithoutTrailingSubstatement
    : Block
    | EmptyStatement
    | ExpressionStatement
    | AssertStatement
    | SwitchStatement
    | DoStatement
    | BreakStatement
    | ContinueStatement
    | ReturnStatement
    | SynchronizedStatement
    | ThrowStatement
    | TryStatement
    | YieldStatement
    ;

EmptyStatement
    : SEMICOLON
    ;

ExpressionStatement
    : StatementExpression SEMICOLON
    ;

AssertStatement
    : ASSERT Expression SEMICOLON
    | ASSERT Expression COLON Expression SEMICOLON
    ;

SwitchStatement
    : SWITCH LPAREN Expression RPAREN SwitchBlock
    ;

DoStatement
    : DO Statement WHILE LPAREN Expression RPAREN SEMICOLON
    ;

BreakStatement
    : BREAK Identifier SEMICOLON
    | BREAK SEMICOLON
    ;

ContinueStatement
    : CONTINUE Identifier SEMICOLON
    | CONTINUE SEMICOLON
    ;

ReturnStatement
    : RETURN Expression SEMICOLON
    | RETURN SEMICOLON
    ;

SynchronizedStatement
    : SYNCHRONIZED LPAREN Expression RPAREN Block
    ;

ThrowStatement
    : THROW Expression SEMICOLON
    ;

TryStatement
    : TRY Block Catches
    | TRY Block OptionalCatches Finally
    | TryWithResourcesStatement
    ;

TryWithResourcesStatement
    : TRY ResourceSpecification Block OptionalCatches OptionalFinally
    ;

ResourceSpecification
    : LPAREN ResourceList OptionalSemicolon RPAREN
    ;

ResourceList
    : Resource
    | Resource SEMICOLON ResourceList
    ;

Resource
    : LocalVariableDeclaration
    | VariableAccess
    ;

OptionalSemicolon
    : /* empty */
    | SEMICOLON
    ;

OptionalCatches
    : /* empty */
    | Catches
    ;

Catches
    : CatchClause
    | Catches CatchClause
    ;

CatchClause
    : CATCH LPAREN CatchFormalParameter RPAREN Block
    ;

CatchFormalParameter
    : VariableModifierList CatchType VariableDeclaratorId
    ;

OptionalFinally
    : /* empty */
    | Finally
    ;

Finally
    : FINALLY Block
    ;

YieldStatement
    : YIELD Expression SEMICOLON
    ;

// Labeled statements
LabeledStatement
    : Identifier COLON Statement
    ;

LabeledStatementNoShortIf
    : Identifier COLON StatementNoShortIf
    ;

// If then statements
IfThenStatement
    : IF LPAREN Expression RPAREN Statement
    ;

IfThenElseStatement
    : IF LPAREN Expression RPAREN StatementNoShortIf ELSE Statement
    ;

IfThenElseStatementNoShortIf
    : IF LPAREN Expression RPAREN StatementNoShortIf ELSE StatementNoShortIf
    ; 

// While statements
WhileStatement
    : WHILE LPAREN Expression RPAREN Statement
    ;

WhileStatementNoShortIf
    : WHILE LPAREN Expression RPAREN StatementNoShortIf
    ;

// For statements
ForStatement
    : BasicForStatement
    | EnhancedForStatement
    ;

ForStatementNoShortIf
    : BasicForStatementNoShortIf
    | EnhancedForStatementNoShortIf
    ;

// Basic for statements
BasicForStatement
    : FOR LPAREN OptionalForInit SEMICOLON OptionalExpression SEMICOLON OptionalForUpdate RPAREN Statement
    ;

BasicForStatementNoShortIf
    : FOR LPAREN OptionalForInit SEMICOLON OptionalExpression SEMICOLON OptionalForUpdate RPAREN StatementNoShortIf
    ;

OptionalForInit
    : /* empty */
    | ForInit
    ;

ForInit
    : StatementExpressionList
    | LocalVariableDeclaration
    ;

OptionalForUpdate
    : /* empty */
    | ForUpdate
    ;

ForUpdate
    : StatementExpressionList
    ;

// Enhanced for statements
EnhancedForStatement
    : FOR LPAREN LocalVariableDeclaration COLON Expression RPAREN Statement
    ;

EnhancedForStatementNoShortIf
    : FOR LPAREN LocalVariableDeclaration COLON Expression RPAREN StatementNoShortIf
    ;

// ============================================================================
// Expressions
// ============================================================================
ParenthesizedExpression
    : LPAREN Expression RPAREN
    ;

Expression
    : LambdaExpression
    | AssignmentExpression
    ;

// Statement expressions
StatementExpressionList
    : StatementExpression
    | StatementExpression COMMA StatementExpressionList
    ;

StatementExpression
    : Assignment
    | PreIncrementExpression
    | PreDecrementExpression
    | PostIncrementExpression
    | PostDecrementExpression
    | MethodInvocation
    | ClassInstanceCreationExpression
    ;

// Lambda expression
LambdaExpression
    : LambdaParameters ARROW LambdaBody
    ;

// Lambda expression - Lambda parameters
LambdaParameters
    : LPAREN LambdaParameterList RPAREN
    | ConciseLambdaParameter
    ;

LambdaParameterList
    : NormalLambdaParameterList
    | ConciseLambdaParameterList
    ;

ConciseLambdaParameterList
    : ConciseLambdaParameter
    | ConciseLambdaParameter COMMA ConciseLambdaParameterList
    ;

ConciseLambdaParameter
    : Identifier
    | "_"
    ;

NormalLambdaParameterList
    : NormalLambdaParameter
    | NormalLambdaParameter COMMA NormalLambdaParameterList
    ;

NormalLambdaParameter
    : VariableModifierList LambdaParameterType VariableDeclaratorId
    | VariableArityParameter
    ;

// Lambda expression - Lambda body
LambdaBody
    : Expression
    | Block
    ;

// Assignment expression
AssignmentExpression // https://docs.oracle.com/javase/specs/jls/se23/html/jls-15.html#jls-AssignmentExpression
    : ConditionalExpression
    | Assignment
    ;

// Assignment expression - Assignment
Assignment
    : LeftHandSide AssignmentOperator Expression
    ;

LeftHandSide
    : ExpressionName
    | FieldAccess
    | ArrayAccess
    ;

FieldAccess
    : Primary DOT Identifier
    | SUPER DOT Identifier
    | TypeName DOT SUPER DOT Identifier
    ;

ArrayAccess
    : ExpressionName LBRACKET Expression RBRACKET
    | PrimaryNoNewArray LBRACKET Expression RBRACKET
    | ArrayCreationExpressionWithInitializer LBRACKET Expression RBRACKET
    ;

VariableAccess
    : Expression
    | FieldAccess
    ;

// Assignment operators
AssignmentOperator
    : ASSIGN
    | TIMES_ASSIGN
    | DIVIDE_ASSIGN
    | MOD_ASSIGN
    | PLUS_ASSIGN
    | MINUS_ASSIGN
    | LEFT_SHIFT_ASSIGN
    | RIGHT_SHIFT_ASSIGN
    | UNSIGNED_RIGHT_SHIFT_ASSIGN
    | BITWISE_AND_ASSIGN
    | BITWISE_XOR_ASSIGN
    | BITWISE_OR_ASSIGN
    ;

// Conditional Expression
ConditionalExpression
    : ConditionalOrExpression
    | ConditionalOrExpression QUESTION_MARK Expression COLON ConditionalExpression
    | ConditionalOrExpression QUESTION_MARK Expression COLON LambdaExpression
    ;

ConditionalOrExpression
    : ConditionalAndExpression
    | ConditionalOrExpression OR ConditionalAndExpression
    ;

ConditionalAndExpression
    : InclusiveOrExpression
    | ConditionalAndExpression AND InclusiveOrExpression
    ;

InclusiveOrExpression
    : ExclusiveOrExpression
    | InclusiveOrExpression BITWISE_OR ExclusiveOrExpression
    ;

ExclusiveOrExpression  
    : AndExpression
    | ExclusiveOrExpression BITWISE_XOR AndExpression
    ;

AndExpression
    : EqualityExpression
    | AndExpression BITWISE_AND EqualityExpression
    ;

EqualityExpression
    : RelationalExpression
    | EqualityExpression EQUALS RelationalExpression
    | EqualityExpression NOT_EQUALS RelationalExpression
    ;

RelationalExpression
    : ShiftExpression
    | RelationalExpression LESS ShiftExpression
    | RelationalExpression GREATER ShiftExpression
    | RelationalExpression LESS_EQUAL ShiftExpression
    | RelationalExpression GREATER_EQUAL ShiftExpression
    | InstanceOfExpression
    ;

ShiftExpression
    : AdditiveExpression
    | ShiftExpression LEFT_SHIFT AdditiveExpression
    | ShiftExpression RIGHT_SHIFT AdditiveExpression
    | ShiftExpression UNSIGNED_RIGHT_SHIFT AdditiveExpression
    ;

AdditiveExpression
    : MultiplicativeExpression
    | AdditiveExpression PLUS MultiplicativeExpression
    | AdditiveExpression MINUS MultiplicativeExpression
    ;

MultiplicativeExpression
    : UnaryExpression
    | MultiplicativeExpression TIMES UnaryExpression
    | MultiplicativeExpression DIVIDE UnaryExpression
    | MultiplicativeExpression MOD UnaryExpression
    ;

UnaryExpression
    : PreIncrementExpression
    | PreDecrementExpression
    | PLUS UnaryExpression
    | MINUS UnaryExpression
    | UnaryExpressionNotPlusMinus
    ;

PreIncrementExpression
    : PLUS_PLUS UnaryExpression
    ;

PreDecrementExpression
    : MINUS_MINUS UnaryExpression
    ;

UnaryExpressionNotPlusMinus
    : PostfixExpression
    | BITWISE_NOT UnaryExpression
    | NOT UnaryExpression
    | CastExpression
    | SwitchExpression
    ;

// Instance of expression
InstanceOfExpression
    : RelationalExpression INSTANCEOF ReferenceType
    | RelationalExpression INSTANCEOF Pattern
    ;

// Switch expression
SwitchExpression
    : SWITCH LPAREN Expression RPAREN SwitchBlock
    ;

SwitchBlock
    : LBRACE SwitchRules RBRACE
    | LBRACE SwitchBlackStatementGroup OptionalSwitchLabels RBRACE
    ;

SwitchRules
    : /* empty */
    | SwitchRule SwitchRules
    ;

SwitchRule
    : SwitchLabel ARROW Expression SEMICOLON
    | SwitchLabel ARROW Block
    | SwitchLabel ARROW ThrowStatement
    ;

SwitchBlockStatementGroup
    : SwitchLabelColon OptionalSwitchLabels BlockStatements
    ;

OptionalSwitchLabels
    : /* empty */
    | SwitchLabelColon OptionalSwitchLabels
    ;

SwitchLabelColon
    : SwitchLabel COLON
    ;

SwitchLabel
    : CASE CaseConstants
    | CASE NULL
    | CASE NULL COMMA DEFAULT
    | CASE CasePatterns OptionalGuard
    | DEFAULT
    ;

CaseConstants
    : CaseConstant
    | CaseConstant COMMA CaseConstants
    ;

CaseConstant
    : ConditionalExpression
    ;

CasePatterns
    : CasePattern
    | CasePattern COMMA CasePatterns
    ;

CasePattern:
    : Pattern
    ;

Pattern:
    : TypePattern
    | RecordPattern
    ;

TypePattern
    : LocalVariableDeclaration
    ;

RecordPattern
    : ReferenceType LPAREN ComponentPatternList RPAREN
    ;

ComponentPatternList
    : ComponentPattern
    | ComponentPattern COMMA ComponentPatternList
    ;

ComponentPattern
    : Pattern
    | MatchAllPattern
    ;

MatchAllPattern
    : "_"
    ;

OptionalGuard
    : /* empty */
    | Guard
    ;

Guard
    : WHEN Expression
    ;

// Cast expression
CastExpression
    : LPAREN PrimitiveType RPAREN UnaryExpression
    | LPAREN ReferenceType AdditionalBoundList RPAREN UnaryExpressionNotPlusMinus
    | LPAREN ReferenceType AdditionalBoundList RPAREN LambdaExpression
    ;

PostfixExpression
    : Primary
    | ExpressionName
    | PostIncrementExpression
    | PostDecrementExpression
    ;

PostIncrementExpression
    : PostfixExpression PLUS_PLUS
    ;

PostDecrementExpression
    : PostfixExpression MINUS_MINUS
    ;

Primary
    : PrimaryNoNewArray
    | ArrayCreationExpression
    ;

PrimaryNoNewArray
    : Literal
    | ClassLiteral
    | THIS
    | TypeName DOT THIS
    | ParenthesizedExpression
    | ClassInstanceCreationExpression
    | FieldAccess
    | ArrayAccess
    | MethodInvocation
    | MethodReference
    ;

// Method reference
MethodReference
    : ExpressionName DOUBLE_COLON OptionalTypeArguments Identifier
    | Primary DOUBLE_COLON OptionalTypeArguments Identifier
    | ReferenceType DOUBLE_COLON OptionalTypeArguments Identifier
    | SUPER DOUBLE_COLON OptionalTypeArguments Identifier
    | TypeName DOT SUPER DOUBLE_COLON OptionalTypeArguments Identifier
    | ClassType DOUBLE_COLON NEW
    | ArrayType DOUBLE_COLON NEW
    ;

// Method invocations
MethodInvocation
    : MethodName LPAREN OptionalArgumentList RPAREN
    | TypeName DOT OptionalTypeArguments Identifier LPAREN OptionalArgumentList RPAREN
    | ExpressionName DOT OptionalTypeArguments Identifier LPAREN OptionalArgumentList RPAREN
    | Primary DOT OptionalTypeArguments Identifier LPAREN OptionalArgumentList RPAREN
    | SUPER DOT OptionalTypeArguments Identifier LPAREN OptionalArgumentList RPAREN
    | TypeName DOT SUPER DOT OptionalTypeArguments Identifier LPAREN OptionalArgumentList RPAREN
    ;

// Class instance creation expression
ClassInstanceCreationExpression
    : UnqualifiedClassInstanceCreationExpression
    | ExpressionName DOT UnqualifiedClassInstanceCreationExpression
    | Primary DOT UnqualifiedClassInstanceCreationExpression
    ;

UnqualifiedClassInstanceCreationExpression
    : NEW OptionalTypeArguments ClassOrInterfaceTypeToInstantiate LPAREN OptionalArgumentList RPAREN OptionalClassBody
    ;

ClassOrInterfaceTypeToInstantiate
    : AnnotatedIdentifierDotList OptionalTypeArgumentsOrDiamond
    ;

AnnotatedIdentifierDotList
    : AnnotatedIdentifier
    | AnnotatedIdentifier DOT AnnotatedIdentifierList
    ;

AnnotatedIdentifier
    : Annotations Identifier
    ;

// Array creation
ArrayCreationExpression
    : ArrayCreationExpressionWithoutInitializer
    | ArrayCreationExpressionWithInitializer
    ;

ArrayCreationExpressionWithoutInitializer
    : NEW PrimitiveType DimExprs OptionalDims
    | NEW ClassOrInterfaceType DimExprs OptionalDims
    ;

ArrayCreationExpressionWithInitializer
    : NEW PrimitiveType Dims ArrayInitializer
    | NEW ClassOrInterfaceType Dims ArrayInitializer
    ;

DimExprs
    : DimExpr
    | DimExpr DimExprs
    ;

DimExpr
    : Annotations LBRACKET Expression RBRACKET
    ;

VariableDeclaratorId
    : Identifier OptionalDims
    | "_"
    ;


LocalVariableDeclaration
    : VariableModifierList LocalVariableType VariableDeclaratorList
    ;

VariableDeclaratorList
    | VariableDeclarator
    | VariableDeclarator COMMA VariableDeclaratorList
    ;

VariableDeclarator
    : VariableDeclaratorId ASSIGN VariableInitializer
    | VariableDeclaratorId
    ;

VariableDeclaratorId
    : Identifier OptionalDims
    | "_"
    ;

VariableInitializer
    : Expression
    | ArrayInitializer
    ;

ArrayInitializer
    : LBRACE VariableInitializerList COMMA RBRACE
    | LBRACE VariableInitializerList RBRACE
    ;

VariableInitializerList
    : VariableInitializer
    | VariableInitializer COMMA VariableInitializerList
    ;

// ============================================================================
// Literals
// ============================================================================
// Class literal
ClassLiteral
    : TypeName OptionalEmptyBrackets DOT CLASS
    | NumericType OptionalEmptyBrackets DOT CLASS
    | BOOLEAN OptionalEmptyBrackets DOT CLASS
    | VOID DOT CLASS
    ;

OptionalEmptyBrackets
    : /* empty */
    | LBRACKET RBRACKET
    ;

// Literals
Literal
    : IntegerLiteral
    | FloatingPointLiteral
    | BooleanLiteral
    | CharacterLiteral
    | STRING_LITERAL // https://docs.oracle.com/javase/specs/jls/se23/html/jls-3.html#jls-StringLiteral
    | TextBlock
    | NullLiteral
    ;

// Integer literals
IntegerLiteral
    : DECIMAL_INTEGER_LITERAL
    | HEX_INTEGER_LITERAL
    | OCTAL_INTEGER_LITERAL
    | BINARY_INTEGER_LITERAL
    ;

// Floating point literals
FloatingPointLiteral
    : DECIMAL_FLOATING_POINT_LITERAL
    | HEX_FLOATING_POINT_LITERAL
    ;

// Boolean literals
BooleanLiteral
    : TRUE
    | FALSE
    ;
// Character literals
// TODO: Figure out how to clean these up
// https://docs.oracle.com/javase/specs/jls/se23/html/jls-3.html#jls-CharacterLiteral
CharacterLiteral
    : RAW_CHARACTER_LITERAL
    | EscapeSequence
    ;

EscapeSequence
    : BACKSLASH "b"
    | BACKSLASH "s"
    | BACKSLASH "t"
    | BACKSLASH "n"
    | BACKSLASH "f"
    | BACKSLASH "r"
    | BACKSLASH LineTerminator
    | BACKSLASH "\""
    | BACKSLASH "'"
    | BACKSLASH BACKSLASH
    | OctalEscape
    ;

OctalEscape
    : BACKSLASH OctalDigit
    | BACKSLASH OctalDigit OctalDigit
    | BACKSLASH ZeroToThree OctalDigit OctalDigit
    ;
// String literals
// Text block
// TODO: https://docs.oracle.com/javase/specs/jls/se23/html/jls-3.html#jls-TextBlock
TextBlock
    : " " " {TextBlockWhiteSpace} LineTerminator {TextBlockCharacter} " " "
    ;

TextBlockWhiteSpace
    : WhiteSpace but not LineTerminator
    ;

TextBlockCharacter
    : InputCharacter but not \
    | EscapeSequence
    | LineTerminator
    ;

// Null literal
NullLiteral
    : NULL
    ;

// Keywords
Keyword
    : ReservedKeyword
    | ContextualKeyword
    ;

ReservedKeyword
    : ABSTRACT
    | ASSERT
    | BOOLEAN
    | BREAK
    | BYTE
    | CASE
    | CATCH
    | CHAR
    | CLASS
    | CONST
    | CONTINUE
    | DEFAULT
    | DO
    | DOUBLE
    | ELSE
    | ENUM
    | EXTENDS
    | FINAL
    | FINALLY
    | FLOAT
    | FOR
    | IF
    | GOTO
    | IMPLEMENTS
    | IMPORT
    | INSTANCEOF
    | INT
    | INTERFACE
    | LONG
    | NATIVE
    | NEW
    | PACKAGE
    | PRIVATE
    | PROTECTED
    | PUBLIC
    | RETURN
    | SHORT
    | STATIC
    | STRICTFP
    | SUPER
    | SWITCH
    | SYNCHRONIZED
    | THIS
    | THROW
    | THROWS
    | TRANSIENT
    | TRY
    | VOID
    | VOLATILE
    | WHILE
    ;

ContextualKeyword
    : EXPORTS
    | MODULE
    | NON_SEALED
    | OPEN
    | OPENS
    | PERMITS
    | PROVIDES
    | RECORD
    | REQUIRES
    | SEALED
    | TO
    | TRANSITIVE
    | USES
    | VAR
    | WHEN
    | WITH
    | YIELD
    ;

// Characters
UnicodeInputCharacter
    : UnicodeEscape
    | RawInputCharacter
    ;

UnicodeEscape
    : BACKSLASH UNICODE_MARKER HexDigit HexDigit HexDigit HexDigit
    ;

HexDigit
    : [0-9a-fA-F]
    ;

OctalDigit
    : [0-7]
    ;

ZeroToThree
    : [0-3]
    ;

LineTerminator
    : "\n"
    // TODO make sure this actually supports all line terminators
    ;

RawInputCharacter
    : .
    ;