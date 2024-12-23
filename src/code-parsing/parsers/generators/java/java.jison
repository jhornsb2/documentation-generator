%{
    // TODO: Define the tokens that will be used in the grammar
%}

%%

// A Java file can be either a package-info.java file or a code file.
JavaFile
    : PackageInfoFile
    | CodeFile
    ;

/*
 * Package Information Files (package-info.java):
 * These files contain information about the package that the code is in.
 * They should consist of a single package delcaration, possibly with a
 * documentation comment.
 */
PackageInfoFile
    : DOCCOMMENT? PackageDeclaration EOF
    ;

/*
 * Code Files (Class, Interface, Enum):
 * These are the files where actual code is written. They can be of the following types:
 * - Class Files
 * - Interface Files
 * - Enum Files
 */
CodeFile
    : ClassFile
    | EnumFile
    | InterfaceFile
    ;

/*
 * Class Files:
 * These files contain the definition of a class.
 */
//ClassFile
//    : PackageDeclaration? ImportDeclaration* DOCCOMMENT? ClassDeclaration EOF
//    ;

/*
 * Enum Files:
 * These files contain the definition of an enum.
 */

/*
 * Interface Files:
 * These files contain the definition of an interface.
 */
InterfaceFile
    : PackageDeclaration? ImportDeclaration* DOCCOMMENT? InterfaceDeclaration EOF
    ;

// TODO: Support extending interfaces
// TODO: Support generic interfaces
// TODO: Support annotations
InterfaceDeclaration
    : VisbilityModifier INTERFACE IDENTIFIER LBRACE InterfaceBodyContent* RBRACE
    ;

InterfaceBodyContent
    : InterfaceMethodDeclaration
    ;

InterfaceMethodDeclaration
    : PUBLIC? MethodDeclaration SEMICOLON
    ;

/*
 * Common Rules
 */
PackageDeclaration
    : PACKAGE QualifiedName SEMICOLON
    ;

ImportDeclaration
    : IMPORT QualifiedName SEMICOLON
    ;

QualifiedName
    : IDENTIFIER (DOT IDENTIFIER)*
    ;

VisbilityModifier
    : /* Package level visibility */
    | PUBLIC
    | PROTECTED
    | PRIVATE
    ;

MethodDeclaration
    : Type IDENTIFIER LPAREN FormalParameterList RPAREN
    ;

FormalParameterList
    : /* Empty */
    | FormalParameter (COMMA FormalParameter)*
    ;

// TODO: Support annotations
FormalParameter
    : Type IDENTIFIER
    ;