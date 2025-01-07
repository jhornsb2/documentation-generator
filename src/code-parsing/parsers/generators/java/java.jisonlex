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
"true"                                          return 'TRUE';
"false"                                         return 'FALSE';

// reserved keywords
"abstract"                                      return 'ABSTRACT';
"assert"                                        return 'ASSERT';
"boolean"                                       return 'BOOLEAN';
"break"                                         return 'BREAK';
"byte"                                          return 'BYTE';
"case"                                          return 'CASE';
"catch"                                         return 'CATCH';
"char"                                          return 'CHAR';
"class"                                         return 'CLASS';
"const"                                         return 'CONST';
"continue"                                      return 'CONTINUE';
"default"                                       return 'DEFAULT';
"do"                                            return 'DO';
"double"                                        return 'DOUBLE';
"else"                                          return 'ELSE';
"enum"                                          return 'ENUM';
"extends"                                       return 'EXTENDS';
"final"                                         return 'FINAL';
"finally"                                       return 'FINALLY';
"float"                                         return 'FLOAT';
"for"                                           return 'FOR';
"goto"                                          return 'GOTO';
"if"                                            return 'IF';
"implements"                                    return 'IMPLEMENTS';
"import"                                        return 'IMPORT';
"instanceof"                                    return 'INSTANCEOF';
"int"                                           return 'INT';
"interface"                                     return 'INTERFACE';
"long"                                          return 'LONG';
"native"                                        return 'NATIVE';
"new"                                           return 'NEW';
"package"                                       return 'PACKAGE';
"private"                                       return 'PRIVATE';
"protected"                                     return 'PROTECTED';
"public"                                        return 'PUBLIC';
"return"                                        return 'RETURN';
"short"                                         return 'SHORT';
"static"                                        return 'STATIC';
"strictfp"                                      return 'STRICTFP';
"super"                                         return 'SUPER';
"switch"                                        return 'SWITCH';
"synchronized"                                  return 'SYNCHRONIZED';
"this"                                          return 'THIS';
"throw"                                         return 'THROW';
"throws"                                        return 'THROWS';
"transient"                                     return 'TRANSIENT';
"try"                                           return 'TRY';
"void"                                          return 'VOID';
"volatile"                                      return 'VOLATILE';
"while"                                         return 'WHILE';

// contextual keywords
"exports"                                       return 'EXPORTS';
"module"                                        return 'MODULE';
"non-sealed"                                    return 'NON_SEALED';
"open"                                          return 'OPEN';
"opens"                                         return 'OPENS';
"permits"                                       return 'PERMITS';
"provides"                                      return 'PROVIDES';
"record"                                        return 'RECORD';
"requires"                                      return 'REQUIRES';
"sealed"                                        return 'SEALED';
"to"                                            return 'TO';
"transitive"                                    return 'TRANSITIVE';
"uses"                                          return 'USES';
"var"                                           return 'VAR';
"when"                                          return 'WHEN';
"with"                                          return 'WITH';
"yield"                                         return 'YIELD';

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
"*="                                            return 'TIMES_ASSIGN';
"/="                                            return 'DIVIDE_ASSIGN';
"%="                                            return 'MOD_ASSIGN';
"+="                                            return 'PLUS_ASSIGN';
"-="                                            return 'MINUS_ASSIGN';
"<<="                                           return 'LEFT_SHIFT_ASSIGN';
">>="                                           return 'RIGHT_SHIFT_ASSIGN';
">>>="                                          return 'UNSIGNED_RIGHT_SHIFT_ASSIGN';
"&="                                            return 'BITWISE_AND_ASSIGN';
"^="                                            return 'BITWISE_XOR_ASSIGN';
"|="                                            return 'BITWISE_OR_ASSIGN';
"+"                                             return 'PLUS';
"++"                                            return 'PLUS_PLUS';
"-"                                             return 'MINUS';
"--"                                            return 'MINUS_MINUS';
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
":"                                             return 'COLON';
"::"                                            return 'DOUBLE_COLON';
"->"                                            return 'ARROW';

"?"                                             return 'QUESTION_MARK';
"@"                                             return 'AT';
"\"                                             return 'BACKSLASH';
"u"                                             return 'UNICODE_MARKER';

<<EOF>>                                         return 'ENDOFFILE';