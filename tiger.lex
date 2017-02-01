type pos = int
type lexresult = Tokens.token

exception LexError of int

fun raiseLexError(pos:int) =
  raise LexError(pos)

fun lexErrorWithPrint(str:string, pos: int) =
  let val dummy = print("[ABOUT TO THROW AN ERROR GG] "^str)
  in
    raiseLexError(pos)
  end

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

fun printLineNum () = 
  let val ln = !lineNum in
  print ("line num is " ^(Int.toString(ln)) ^"\n")
  end

val inString = ref false
val inComment = ref false

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

fun strFromCtrl str =
    let val charOpt = Char.fromString(str)
    in
    if isSome(charOpt) then 
    (addToBuffer(Char.toString(valOf charOpt));
     print "Added escaped control character to string buffer\n")
    else
      (print "Illegal control character sequence. Ignoring.\n")
    end  

%%
  %s COMMENT STRING IGNORESEQ;
  alpha=[A-Za-z];
  digit=[0-9];
  ws=[\ \t];
  identifier={alpha}[A-Za-z0-9_]*;
  nonprintable=(\t | " " | \f);
%%
<INITIAL>\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>type	=> (Tokens.TYPE(yypos,yypos+4));
<INITIAL>{ws}+ => (continue());
<INITIAL>","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL>var  	=> (printLineNum(); Tokens.VAR(yypos,yypos+3));
<INITIAL>":=" => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL>{digit}+ => (Tokens.INT(Option.valOf(Int.fromString(yytext)), yypos, yypos+String.size(yytext) ));
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
<INITIAL>"if" => (Tokens.IF(yypos,yypos+2));
<INITIAL>"array" => (Tokens.ARRAY(yypos,yypos+5));
<INITIAL>"while" => (Tokens.WHILE(yypos, yypos + size yytext));
<INITIAL>"else" => (Tokens.ELSE(yypos, yypos + size yytext));
<INITIAL>"then" => (Tokens.THEN(yypos, yypos + size yytext));
<INITIAL>{identifier} => (Tokens.ID(yytext, yypos, (yypos + String.size(yytext)) ));
<INITIAL>"/*" => (YYBEGIN COMMENT; print "Entering COMMENT\n"; inComment := true; incrNest(); continue());
<COMMENT> "/*" => (print "Incrementing comment nestedness.\n"; inComment := true; incrNest();continue());
<COMMENT>\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT>"*/" => (print "Uncomment detected.\n"; if decrNest() = 0 then(YYBEGIN(INITIAL); inComment := false; print "Returning to INITIAL from COMMENT\n") else(print("Still in COMMENT, nesting: " ^ Int.toString(!commNest) ^ "\n")); continue());
<COMMENT>. => (continue());
<INITIAL>"\""=> (YYBEGIN STRING; inString := true; print "String starting\n"; continue());
<STRING>"\""=> (YYBEGIN INITIAL;print "String ending\n"; inString := false; Tokens.STRING(!strBuffer, yypos - 1 - clearBuffer(), yypos + 1));
<STRING>"\\" => (YYBEGIN IGNORESEQ; print "Entering IGNORESEQ state\n"; continue());
<STRING>"\\""\\" => (print "Printing literal backslash character.\n"; addToBuffer "\\"; continue());
<STRING>"\\""\"" => (print "Printing literal \" character within string\n"; addToBuffer "\""; continue());
<STRING>"\\^". => (print "Escaping control character\n"; strFromCtrl(yytext); continue());
<STRING>"\\"{digit}{digit}{digit} => (print("Interpreting ascii character sequence " ^ yytext ^ "\n"); addToBuffer(Char.toString(chr(valOf (Int.fromString(String.extract(yytext, 1, NONE)))))); continue());
<STRING>("\\n" | "\\t" | " " | "\\f" | [^"\\"]) => (addToBuffer yytext; continue());
<STRING>{digit}+ => (print "Printing integer literal within string\n"; addToBuffer yytext; continue());
<STRING>. => (ErrorMsg.error yypos ("Illegal use of \\ character."); continue());
<IGNORESEQ>"\\" => (YYBEGIN STRING; print "Returning to STRING state from IGNORESEQ state\n"; continue());
<IGNORESEQ>(\n) => (lineNum := !lineNum+1; print "incrementing lineNum in IGNORESEQ\n"; linePos := yypos :: !linePos; continue());
<IGNORESEQ>{nonprintable} => (print "got nonprintable in IGNORESEQ\n";continue());
<IGNORESEQ>. => (print "Printable character received from IGNORESEQ"; ErrorMsg.error yypos ("illegal use of printable character from IGNORESEQ"); continue());
<INITIAL> "<>" => (Tokens.NEQ(yypos,yypos+2));
<INITIAL> "|" => (Tokens.OR(yypos,yypos+2));
<INITIAL> "&" => (Tokens.AND(yypos,yypos+2));
<INITIAL> ">=" => (Tokens.GE(yypos,yypos+2));
<INITIAL> "<=" => (Tokens.LE(yypos,yypos+2));
<INITIAL> ">" => (Tokens.GT(yypos,yypos+1));
<INITIAL> "<" => (Tokens.LT(yypos,yypos+1));
<INITIAL> "=" => (Tokens.EQ(yypos,yypos+1));
<INITIAL> "/" => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL> "*" => (Tokens.TIMES(yypos,yypos+1));
<INITIAL> "-" => (Tokens.MINUS(yypos,yypos+1));
<INITIAL> "+" => (Tokens.PLUS(yypos,yypos+1));
<INITIAL> "." => (Tokens.DOT(yypos,yypos+1));
<INITIAL> "}" => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL> "{" => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL> "]" => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL> "[" => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL> ")" => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL> "(" => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL> ";" => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL> ":" => (Tokens.COLON(yypos,yypos+1));
<INITIAL> "," => (Tokens.COMMA(yypos,yypos+1));
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
