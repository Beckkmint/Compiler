import java_cup.runtime.*;

%%

%class Lexer
%unicode
%line
%column
%cup

%{
    private Symbol symbol(int type) {
        return new Symbol(type, yyline, yycolumn);
    }
  
    private Symbol symbol(int type, Object value) {
        return new Symbol(type, yyline, yycolumn, value);
    }
%}

LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]
WhiteSpace     = {LineTerminator} | [ \t\f]

/* comments */
Comment = {TraditionalComment} | {EndOfLineComment}

TraditionalComment   = "/#" [^#] ~"#/" | "/#" "#" + "/"
EndOfLineComment     = "#" {InputCharacter}* {LineTerminator}?

Identifier = [a-zA-Z][a-zA-Z0-9_]*

Boolean = T | F
Punctuation = [!\"#\$%&\'()\*\+\,\-\.\/:;<=>\?@\[\]\\\^_`{}\~¦]
Character = '{Punctuation}' | '[0-9]' | '[a-zA-Z]'

Integer = 0 | [1-9][0-9]*
Rational = ({Integer}_)[0-9]"/"[1-9][0-9]* | {Integer}"/"[1-9][0-9]*
Float = {Integer}(\.[0-9]+)?
Number = {Integer} | {Rational} | {Float}

String = \"(\\.|[^\"])*\"


%%

<YYINITIAL> {

  /* keywords */
  "main"                         { return symbol(sym.MAIN); }
  "alias"                        { return symbol(sym.ALIAS); }
  "tdef"                         { return symbol(sym.TDEF); }
  "fdef"                         { return symbol(sym.FDEF); }
  "in"                           { return symbol(sym.IN); }
  "return"                       { return symbol(sym.RETURN); }
  "read"                         { return symbol(sym.READ); }
  "print"                        { return symbol(sym.PRINT); }
  "if"                           { return symbol(sym.IF); }
  "then"                         { return symbol(sym.THEN); }
  "else"                         { return symbol(sym.ELSE); }
  "elif"                         { return symbol(sym.ELIF); }
  "fi"                           { return symbol(sym.FI); }
  "forall"                       { return symbol(sym.FORALL); }
  "while"                        { return symbol(sym.WHILE); }
  "do"                           { return symbol(sym.DO); }
  "od"                           { return symbol(sym.OD); }
  "break"                        { return symbol(sym.BREAK); }
  "loop"                         { return symbol(sym.LOOP); }
  "pool"                         { return symbol(sym.POOL); }

  /* types */
  "char"                         { return symbol(sym.CHAR); }
  "int"                          { return symbol(sym.INTEGER); }
  "rat"                          { return symbol(sym.RATIONAL); }
  "float"                        { return symbol(sym.FLOAT); }
  "dict"                         { return symbol(sym.DICT); }
  "seq"                          { return symbol(sym.SEQ); }
  "set"                          { return symbol(sym.SET); }
  "top"                          { return symbol(sym.TOP); }
  "thread"                       { return symbol(sym.THREAD); }
  "function"                     { return symbol(sym.FUNCTION); }

  /* separators */
  "("                            { return symbol(sym.LPAREN); }
  ")"                            { return symbol(sym.RPAREN); }
  "{"                            { return symbol(sym.LBRACE); }
  "}"                            { return symbol(sym.RBRACE); }
  "["                            { return symbol(sym.LBRACK); }
  "]"                            { return symbol(sym.RBRACK); }
  ","                            { return symbol(sym.COMMA); }
  "."                            { return symbol(sym.DOT); }
  ";"                            { return symbol(sym.SEMI); }
  ":"                            { return symbol(sym.COLON); }

  /* operators */
  ":="                           { return symbol(sym.EQ); }
  "<"                            { return symbol(sym.LT); }
  ">"                            { return symbol(sym.GT); }
  "<="                           { return symbol(sym.LTEQ); }
  ">="                           { return symbol(sym.GTEQ); }
  "=="                           { return symbol(sym.EQEQ); }
  "!="                           { return symbol(sym.NOTEQ); }
  "+"                            { return symbol(sym.PLUS); }
  "-"                            { return symbol(sym.MINUS); }
  "*"                            { return symbol(sym.MULT); }
  "/"                            { return symbol(sym.DIV); }
  "^"                            { return symbol(sym.XOR); }
  "::"                           { return symbol(sym.CONCAT); }
  "!"                            { return symbol(sym.NOT); }
  "&&"                           { return symbol(sym.AND); }
  "||"                           { return symbol(sym.OR); }
  "&"                            { return symbol(sym.INTERSECTION); }
  "|"                            { return symbol(sym.UNION); }
  "\\"                           { return symbol(sym.DIFFERENCE); }
  "->"                           { return symbol(sym.IMPLY); }
 
  /* literals */
  {Boolean}                      { return symbol(sym.BOOLEAN); }
  {String}                       { return symbol(sym.STRING); }
  {Number}                       { return symbol(sym.NUMBER); }
  {Character}                    { return symbol(sym.CHAR); }

  /* identifiers */
  {Identifier}                   { return symbol(sym.IDENTIFIER); }

  /* comments */
  {Comment}                      { /* ignore */ }
 
  /* whitespace */
  {WhiteSpace}                   { /* ignore */ }
}

/* error fallback */
[^]                              { throw new Error("Illegal character <"+ yytext()+">"); }