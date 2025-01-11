// shortcut declarations
underscores                 _+
id                          [a-zA-Z_][a-zA-Z0-9_]*
whitespace                  \s+

zeroToThree                 [0-3]

// decimal numerals
nonzeroDigit                [1-9]
digit                       0|{nonzeroDigit}
digitOrUnderscore           [0-9_]
digitsAndUnderscores        {digitOrUnderscore}+
digits                      {digit} | ({digit}{digitsAndUnderscores}{digit})
decimalNumeral              0 | ({nonzeroDigit}{digits}*) | ({nonzeroDigit}{underscores}{digits})
// hex numerals
hexDigit                    [0-9a-fA-F]
hexDigitOrUnderscore        {hexDigit} | _
hexDigitOrUnderscores       {hexDigitOrUnderscore}+
hexDigits                   {hexDigit} | ({hexDigit}{hexDigitOrUnderscores}{hexDigit})
hexNumeral                  0[xX]{hexDigits}
// octal numerals
octalDigit                  [0-7]
octalDigitOrUnderscore      {octalDigit} | _
octalDigitsAndUnderscores   {octalDigitOrUnderscore}+
octalDigits                 {octalDigit} | ({octalDigit}{octalDigitsAndUnderscores}{octalDigit})
octalNumeral                (0{octalDigits}) | (0{underscores}{octalDigits})
// binary numerals
binaryDigit                 [01]
binaryDigitOrUnderscore     {binaryDigit} | _
binaryDigitsAndUnderscores  {binaryDigitOrUnderscore}+
binaryDigits                {binaryDigit} | ({binaryDigit}{binaryDigitsAndUnderscores}{binaryDigit})
binaryNumeral               0[bB]{binaryDigits}

// suffixes
integerTypeSuffix           [lL]
floatTypeSuffix             [fFdD]
exponentIndicator           [eE]
binaryExponentIndicator     [pP]
sign                        [+-]
dot                         \.

signedInteger               {sign}?{digits}

// exponent parts
exponentPart                {exponentIndicator}{signedInteger}
binaryExponent              {binaryExponentIndicator}{signedInteger}

hexSignificand              ({hexNumeral}{dot}?) | 0[xX]{hexDigits}?{dot}{hexDigits}

// character and string support
singleQuote                 \'
doubleQuote                 \"
textBlockDelimiter          {doubleQuote}{doubleQuote}{doubleQuote}
textBlockWhitespace         (?!{lineTerminator}){whitespace}

lineTerminator              (\r\n) | [\r\n]

rawInputCharacter           .
unicodeMarker               u+
unicodeEscape               \\{unicodeMarker}{hexDigit}{hexDigit}{hexDigit}{hexDigit}
unicodeInputCharacter       {unicodeEscape} | {rawInputCharacter}
inputCharacter              (?![\r\n]){unicodeInputCharacter}
singleCharacter             (?!['\\]){inputCharacter}

octalEscape                 \\(({octalDigit})|({octalDigit}{octalDigit})|({zeroToThree}{octalDigit}{octalDigit}))
escapeSequence              (\\([bstnfr"'\\]|{lineTerminator}))|{octalEscape}

stringCharacter             (?!["\\]){inputCharacter} | {escapeSequence}
textBlockCharacter          (?![\\]){inputCharacter} | {escapeSequence} | {lineTerminator}

%%

// ignore whitespace and comments
{whitespace}                                  /* ignore whitespace */
"//".*                                        /* ignore single-line comment */
\/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*\/    /* ignore multi-line comment */

// documentation comments
\/\*\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*\/    return 'DOC_COMMENT';

// literals
// integer literals
{decimalNumeral}{integerTypeSuffix}?            return 'DECIMAL_INTEGER_LITERAL';
{hexNumeral}{integerTypeSuffix}?                return 'HEX_INTEGER_LITERAL';
{octalNumeral}{integerTypeSuffix}?              return 'OCTAL_INTEGER_LITERAL';
{binaryNumeral}{integerTypeSuffix}?             return 'BINARY_INTEGER_LITERAL';
// floating point literals
{digits}{dot}{digits}?{exponentPart}?{floatTypeSuffix}?   return 'DECIMAL_FLOATING_POINT_LITERAL';
{dot}{digits}{exponentPart}?{floatTypeSuffix}?            return 'DECIMAL_FLOATING_POINT_LITERAL';
{digits}{exponentPart}{floatTypeSuffix}?                  return 'DECIMAL_FLOATING_POINT_LITERAL';
{digits}{exponentPart}?{floatTypeSuffix}                  return 'DECIMAL_FLOATING_POINT_LITERAL';
{hexSignificand}{binaryExponent}{floatTypeSuffix}?        return 'HEX_FLOATING_POINT_LITERAL';
// string and character literals
{singleQuote}{singleCharacter}{singleQuote}     return 'SINGLE_CHARACTER_LITERAL';
{singleQuote}{escapeSequence}{singleQuote}      return 'ESCAPE_SEQUENCE_CHARACTER_LITERAL';
{doubleQuote}{stringCharacter}*{doubleQuote}    return 'STRING_LITERAL';
{textBlockDelimiter}{textBlockWhitespace}*{lineTerminator}{textBlockCharacter}*{textBlockDelimiter} return 'TEXT_BLOCK_LITERAL';
// boolean literals
"true"                                          return 'TRUE';
"false"                                         return 'FALSE';
// null literal
"null"                                          return 'NULL';

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
{dot}                                           return 'DOT';
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

<<EOF>>                                         return 'END_OF_FILE';