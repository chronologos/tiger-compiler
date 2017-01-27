type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

  val inString = ref false;
  val inComment = ref false;

fun err(p1,p2) = ErrorMsg.error p1
fun eof() = let val pos = hd(!linePos) in 
  if !inString then ErrorMsg.error pos ("Unclosed string at EOF") else (); 
  if !inComment then ErrorMsg.error pos ("Unclosed comment at EOF") else ();
  Tokens.EOF(pos,pos) 
end

val strBuffer = ref ""

fun addToBuffer ch = let val oldStr = !strBuffer in strBuffer := oldStr ^ ch end

fun clearBuffer() = 
  let val cleared:string = "" 
  in 
    let
      val strSize = size (!strBuffer) 
    in
      strBuffer := cleared; 
      strSize 
    end
  end

val commNest = ref 0;
fun incrNest() = (commNest := !commNest + 1);
fun decrNest() = (commNest := !commNest - 1; !commNest)

%%
  %s COMMENT STRING IGNORESEQ;
  alpha=[A-Za-z];
  digit=[0-9];
  ws = [\ \t];
  identifier=[A-Za-z0-9_]*;
%%
<INITIAL>\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>"type"	=> (Tokens.TYPE(yypos,yypos+4));
<INITIAL>"("	=> (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"	=> (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>";"	=> (continue());
<INITIAL>{ws}+ => (continue());
<INITIAL>","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL>var  	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL>":=" => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL>{digit}+ => (Tokens.INT(Option.valOf(Int.fromString(yytext)), yypos, yypos+String.size(yytext) ));
<INITIAL>"123"	=> (Tokens.INT(123,yypos,yypos+3));
<INITIAL>function => (Tokens.FUNCTION(yypos, yypos + size yytext));
<INITIAL>"break" => (Tokens.BREAK(yypos, yypos + size yytext));
<INITIAL>"of" => (Tokens.OF(yypos, yypos + size yytext));
<INITIAL>"end" => (Tokens.END(yypos, yypos + size yytext));
<INITIAL>"in" => (Tokens.IN(yypos, yypos + size yytext));
<INITIAL>"nil" => (Tokens.NIL(yypos, yypos + size yytext));
<INITIAL>"let" => (Tokens.LET(yypos, yypos + size yytext));
<INITIAL>"do" => (Tokens.DO(yypos, yypos + size yytext));
<INITIAL>"to" => (Tokens.TO(yypos, yypos + size yytext));
<INITIAL>"for" => (Tokens.FOR(yypos, yypos + size yytext));
<INITIAL>"while" => (Tokens.WHILE(yypos, yypos + size yytext));
<INITIAL>"else" => (Tokens.ELSE(yypos, yypos + size yytext));
<INITIAL>"then" => (Tokens.THEN(yypos, yypos + size yytext));
<INITIAL>{identifier} => (Tokens.ID(yytext, yypos, (yypos + String.size(yytext)) ));
<INITIAL>"/*" => (YYBEGIN COMMENT; print "Entering COMMENT\n"; inComment := true; incrNest(); continue());
<COMMENT> "/*" => (print "Incrementing comment nestedness.\n"; inComment := true; incrNest();continue());
<COMMENT>"*/" => (print "Uncomment detected.\n"; if decrNest() = 0 then(YYBEGIN(INITIAL); inComment := false; print "Returning to INITIAL from COMMENT\n") else(print("Still in COMMENT, nesting: " ^ Int.toString(!commNest) ^ "\n")); continue());
<COMMENT>. => (continue());
<INITIAL>"\""=> (YYBEGIN STRING; inString := true; print "String starting\n"; continue());
<STRING>"\""=> (YYBEGIN INITIAL;print "String ending\n"; inString := false; Tokens.STRING(!strBuffer, yypos - 1 - clearBuffer(), yypos + 1));
<STRING>"\\"(\n | \t | " " | \f)+ => (YYBEGIN IGNORESEQ; print "Entering IGNORESEQ state\n"; continue());
<STRING>"\\""\\" => (print "Printing literal backslash character.\n"; addToBuffer "\\"; continue());
<STRING>("\\n" | "\\t" | " " | "\\f" | [^"\\"]) => (addToBuffer yytext; continue());
<STRING>{digit}+ => (print "Printing integer literal within string\n"; addToBuffer yytext; continue());
<STRING>. => (ErrorMsg.error yypos ("Illegal use of \\ character."); continue());
<IGNORESEQ>"\\" => (YYBEGIN STRING; print "Returning to STRING state from IGNORESEQ state\n"; continue());
<IGNORESEQ>. => (print "STAYING IN IGNORESEQ\n"; continue());
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
