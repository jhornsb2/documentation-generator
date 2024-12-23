// shortcut declarations
digit                       [0-9]
id                          [a-zA-Z_][a-zA-Z0-9_]*
whitespace                  \s+

%%

// ignore whitespace and comments
{whitespace}                                  /* ignore whitespace */
"//".*                                        /* ignore single-line comment */
\/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*\/    /* ignore multi-line comment */

// documentation comments
\/\*\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*\/    return 'DOCCOMMENT';

// literals
{digit}+                                        return 'INTEGER_LITERAL';
{digit}+(\.{digit}+)?([eE][+-]?{digit}+)?[fF]   return 'FLOAT_LITERAL';
{digit}+(\.{digit}+)?([eE][+-]?{digit}+)?[dD]?  return 'DOUBLE_LITERAL';
\"([^\"\\]|\\.)*\"                              return 'STRING_LITERAL';
"null"                                          return 'NULL';

// primitive types
"boolean"                                       return 'BOOLEAN';
"byte"                                          return 'BYTE';
"char"                                          return 'CHAR';
"double"                                        return 'DOUBLE';
"float"                                         return 'FLOAT';
"int"                                           return 'INT';
"long"                                          return 'LONG';
"short"                                         return 'SHORT';
"void"                                          return 'VOID';

// type declaration keywords
"class"                                         return 'CLASS';
"enum"                                          return 'ENUM';
"interface"                                     return 'INTERFACE';

// type inheritance keywords
"extends"                                       return 'EXTENDS';
"implements"                                    return 'IMPLEMENTS';

// access modifiers
"package"                                       return 'PACKAGE';
"private"                                       return 'PRIVATE';
"protected"                                     return 'PROTECTED';
"public"                                        return 'PUBLIC';

// control flow keywords
"break"                                         return 'BREAK';
"case"                                          return 'CASE';
"continue"                                      return 'CONTINUE';
"else"                                          return 'ELSE';
"goto"                                          return 'GOTO';
"if"                                            return 'IF';
"switch"                                        return 'SWITCH';

// error handling keywords
"assert"                                        return 'ASSERT';
"catch"                                         return 'CATCH';
"finally"                                       return 'FINALLY';
"try"                                           return 'TRY';
"throw"                                         return 'THROW';

// loop keywords
"do"                                            return 'DO';
"for"                                           return 'FOR';
"while"                                         return 'WHILE';

// modifier keywords
"abstract"                                      return 'ABSTRACT';
"const"                                         return 'CONST';
"final"                                         return 'FINAL';
"native"                                        return 'NATIVE';
"static"                                        return 'STATIC';
"strictfp"                                      return 'STRICTFP';
"throws"                                        return 'THROWS';
"transient"                                     return 'TRANSIENT';

// reference keywords
"super"                                         return 'SUPER';
"this"                                          return 'THIS';

// miscellaneous keywords
"default"                                       return 'DEFAULT';
"import"                                        return 'IMPORT';
"instanceof"                                    return 'INSTANCEOF';
"new"                                           return 'NEW';
"return"                                        return 'RETURN';
"synchronized"                                  return 'SYNCHRONIZED';
"volatile"                                      return 'VOLATILE';

// identifiers can be a type or a variable name
{id}                                            return 'IDENTIFIER';

// brackets
"("                                             return 'LPAREN';
")"                                             return 'RPAREN';
"{"                                             return 'LBRACE';
"}"                                             return 'RBRACE';
"["                                             return 'LBRACKET';
"]"                                             return 'RBRACKET';

// operators
"=="                                            return 'EQUALS';
"!="                                            return 'NOT_EQUALS';
"="                                             return 'ASSIGN';
"+"                                             return 'PLUS';
"-"                                             return 'MINUS';
"*"                                             return 'TIMES';
"/"                                             return 'DIVIDE';
"%"                                             return 'MOD';
"&&"                                            return 'AND';
"||"                                            return 'OR';
"!"                                             return 'NOT';
"<"                                             return 'LESS';
">"                                             return 'GREATER';
"<="                                            return 'LESS_EQUAL';
">="                                            return 'GREATER_EQUAL';
";"                                             return 'SEMICOLON';
","                                             return 'COMMA';
"."                                             return 'DOT';
"&"                                             return 'BITWISE_AND';
"|"                                             return 'BITWISE_OR';
"^"                                             return 'BITWISE_XOR';
"~"                                             return 'BITWISE_NOT';
"<<"                                            return 'LEFT_SHIFT';
">>"                                            return 'RIGHT_SHIFT';
">>>"                                           return 'UNSIGNED_RIGHT_SHIFT';

<<EOF>>                                         return 'ENDOFFILE';