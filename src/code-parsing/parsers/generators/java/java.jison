%{
    // TODO: Define the tokens that will be used in the grammar
%}

%%

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
    : ClassModifiers CLASS TypeIdentifier TypeParameters ClassExtends OptionalClassImplements ClassPermits ClassBody
    ;

// Enum declaration
EnumDeclaration
    : ClassModifiers ENUM TypeIdentifier OptionalClassImplements EnumBody
    ;

// Record declaration
RecordDeclaration
    : ClassModifiers RECORD TypeIdentifier TypeParameters RecordHeader OptionalClassImplements RecordBody
    ;

// Interface declaration
InterfaceDeclaration
    : NormalInterfaceDeclaration
    | AnnotationInterfaceDeclaration
    ;

// Normal interface declaration
NormalInterfaceDeclaration
    : InterfaceModifiers INTERFACE TypeIdentifier TypeParameters OptionalInterfaceExtends InterfacePermits InterfaceBody
    ;

// Annotation interface declaration
AnnotationInterfaceDeclaration
    : InterfaceModifiers AT INTERFACE TypeIdentifier AnnotationInterfaceBody
    ;

// ============================================================================
// Modifiers
// ============================================================================
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
// ============================================================================
// Expressions
// ============================================================================

// ============================================================================
// Statements
// ============================================================================
// Default value
OptionalDefaultValue
    : /* empty */
    | DefaultValue
    ;

DefaultValue
    : DEFAULT ElementValue
    ;


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

OptionalParenthesizedArguments
    : /* empty */
    | LPAREN OptionalArgumentList RPAREN
    ;

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

// Constructor declarator
ConstructorDeclarator
    : TypeParameters SimpleTypeName LPAREN ReceiverParameter COMMA FormalParameterList RPAREN
    | SimpleTypeName LPAREN ReceiverParameter COMMA FormalParameterList RPAREN
    | TypeParameters SimpleTypeName LPAREN FormalParameterList RPAREN
    | SimpleTypeName LPAREN FormalParameterList RPAREN
    | TypeParameters SimpleTypeName LPAREN RPAREN
    | SimpleTypeName LPAREN RPAREN
    ;

MethodBody
    : Block
    | SEMICOLON
    ;

MethodHeader
    : Result MethodDeclarator OptionalThrows
    | TypeParameters Annotations Result MethodDeclarator OptionalThrows
    ;

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

Result
    : UnannType
    | VOID
    ;

MethodDeclarator
    : Identifier LPAREN ReceiverParameter COMMA FormalParameterList RPAREN Dims
    | Identifier LPAREN ReceiverParameter COMMA FormalParameterList RPAREN
    | Identifier LPAREN ReceiverParameter COMMA RPAREN Dims
    | Identifier LPAREN ReceiverParameter COMMA RPAREN
    | Identifier LPAREN FormalParameterList RPAREN Dims
    | Identifier LPAREN FormalParameterList RPAREN
    | Identifier LPAREN RPAREN Dims
    | Identifier LPAREN RPAREN
    ;

ReceiverParameter
    : Annotations UnannType Identifier DOT THIS
    | Annotations UnannType THIS
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

WildcardBounds
    : /* empty */
    | EXTENDS ReferenceType
    | SUPER ReferenceType
    ;

// Dims
Dims
    : Annotations LBRACKET RBRACKET
    | Annotations LBRACKET RBRACKET Dims
    ;

// Type parameters
TypeParameters
    : /* empty */
    | LT TypeParameterList GT
    ;

TypeParameterList
    : TypeParameter
    | TypeParameter COMMA TypeParameterList
    ;

TypeParameter
    : TypeParameterModifiers TypeIdentifier TypeBound
    ;

TypeParameterModifiers
    : /* empty */
    | TypeParameterModifier TypeParameterModifiers
    ;

TypeParameterModifier
    : Annotation
    ;

TypeBound
    : EXTENDS TypeVariable
    | EXTENDS ClassOrInterfaceType AdditionalBoundList
    ;

AdditionalBoundList
    : /* empty */
    | AdditionalBound AdditionalBoundList
    ;

AdditionalBound
    : BITWISE_AND InterfaceType
    ;

// Define the TypeIdentifier rule using this: https://docs.oracle.com/javase/specs/jls/se23/html/jls-3.html#jls-TypeIdentifier
TypeIdentifier
    : Identifier
    ;

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

MethodName:
    : UnqualifiedMethodIdentifier
    ;

UnqualifiedMethodIdentifier
    : Identifier
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

OptionalArgumentList
    : /* empty */
    | ArgumentList
    ;

ArgumentList
    : Expression
    : Expression COMMA ArgumentList
    ;

// Expressions
ParenthesizedExpression
    : LPAREN Expression RPAREN
    ;

Expression
    : LambdaExpression
    | AssignmentExpression
    ;

// Assignment expression
// https://docs.oracle.com/javase/specs/jls/se23/html/jls-15.html#jls-AssignmentExpression
AssignmentExpression
    : ConditionalExpression
    | Assignment
    ;

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

// Lambda expression
LambdaExpression
    : LambdaParameters ARROW LambdaBody
    ;

// Lambda Parameters
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

VariableModifierList
    : /* empty */
    | VariableModifier
    | VariableModifier VariableModifierList
    ;

VariableModifier
    : ANNOTATIONS
    | FINAL
    ;

VariableDeclaratorId
    : Identifier OptionalDims
    | "_"
    ;

OptionalDims
    : /* empty */
    | Dims
    ;

LambdaParameterType
    : UnannType
    | VAR
    ;

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

// Lambda body
LambdaBody
    : Expression
    | Block
    ;
// Block
Block
    : LBRACE BlockStatements RBRACE
    ;

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

LocalVariableDeclaration
    : VariableModifierList LocalVariableType VariableDeclaratorList
    ;

LocalVariableType
    : UnannType
    | VAR
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

// Statement TODO
Statement
    : // TODO: https://docs.oracle.com/javase/specs/jls/se23/html/jls-14.html#jls-Statement
    ;
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
    : DecimalIntegerLiteral
    | HexIntegerLiteral
    | OctalIntegerLiteral
    | BinaryIntegerLiteral
    ;

// TODO: Figure out how to clean these up
// Do they need to be in jisonlex?
// https://docs.oracle.com/javase/specs/jls/se23/html/jls-3.html#jls-IntegerLiteral
DecimalIntegerLiteral
    : DecimalNumeral OptionalIntegerTypeSuffix
    ;

HexIntegerLiteral
    : HexNumeral OptionalIntegerTypeSuffix
    ;

OctalIntegerLiteral
    : OctalNumeral OptionalIntegerTypeSuffix
    ;

BinaryIntegerLiteral
    : BinaryNumeral OptionalIntegerTypeSuffix
    ;

OptionalIntegerTypeSuffix
    : /* empty */
    | IntegerTypeSuffix
    ;

IntegerTypeSuffix
    : "L"
    | "l"
    ;
// End TODO
// Floating point literals
FloatingPointLiteral
    : DecimalFloatingPointLiteral
    | HexadecimalFloatingPointLiteral
    ;

/*
https://docs.oracle.com/javase/specs/jls/se23/html/jls-3.html#jls-FloatingPointLiteral

    DecimalFloatingPointLiteral:
        Digits . [Digits] [ExponentPart] [FloatTypeSuffix]
        . Digits [ExponentPart] [FloatTypeSuffix]
        Digits ExponentPart [FloatTypeSuffix]
        Digits [ExponentPart] FloatTypeSuffix
  
    ExponentPart:
        ExponentIndicator SignedInteger

    ExponentIndicator:
        (one of)
        e E

    SignedInteger:
        OptionalSign Digits

    OptionalSign
        :
        | Sign
        ;

    Sign
        : PLUS
        | MINUS
        ;

    FloatTypeSuffix:
        (one of)
        f F d D

    HexadecimalFloatingPointLiteral:
        HexSignificand BinaryExponent [FloatTypeSuffix]

    HexSignificand:
        HexNumeral [.]
        0 x [HexDigits] . HexDigits
        0 X [HexDigits] . HexDigits

    BinaryExponent:
        BinaryExponentIndicator SignedInteger

    BinaryExponentIndicator:
        (one of)
        p P
*/

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