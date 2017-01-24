type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

fun err(p1,p2) = ErrorMsg.error p1
fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%%
%s COMMENT
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%
<INITIAL>\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>{ws}+ => (continue());
<INITIAL>","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL>var  	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL>"/*" => (YYBEGIN COMMENT; continue());
<INITIAL>":=" => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL>{digit} => (Tokens.INT(Option.valOf(Int.fromString(yytext)), yypos, yypos+String.size(yytext) ));
<COMMENT>. => (continue());
<COMMENT>"*/" => (YYBEGIN INITIAL; continue());
<INITIAL>"123"	=> (Tokens.INT(123,yypos,yypos+3));
<INITIAL>[A-za-z_][A-Za-z0-9_]* => (Tokens.ID(yytext, yypos, (yypos + String.size(yytext)) ));
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
