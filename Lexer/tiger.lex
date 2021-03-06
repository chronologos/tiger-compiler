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

    fun strFromCtrl(pos, str) =
    let val charOpt = Char.fromString(str)
    in
    if isSome(charOpt) then 
    addToBuffer(Char.toString(valOf charOpt))
    else
      (ErrorMsg.error pos "Illegal control character sequence. Ignoring.\n")
    end  

	fun strFromAscii(pos, str) =
	let val asciiInt = valOf(Int.fromString(String.extract(str, 1, NONE)))
	in
	if asciiInt >= 0 andalso asciiInt <= 255 then addToBuffer(Char.toString(chr(asciiInt))) 
        else ErrorMsg.error pos "Illegal ascii value, not within 0 to 255. Ignoring \\ddd sequence." 
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
<INITIAL>var  	=> (Tokens.VAR(yypos,yypos+3));
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
<INITIAL>"/*" => (YYBEGIN COMMENT; inComment := true; incrNest(); continue());
<COMMENT> "/*" => (inComment := true; incrNest();continue());
<COMMENT>\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT>"*/" => (if decrNest() = 0 then(YYBEGIN(INITIAL); inComment := false) else(); continue());
<COMMENT>. => (continue());
<INITIAL>"\""=> (YYBEGIN STRING; inString := true; continue());
<STRING>"\""=> (YYBEGIN INITIAL;inString := false; Tokens.STRING(!strBuffer, yypos - 1 - clearBuffer(), yypos + 1));
<STRING>"\\" => (YYBEGIN IGNORESEQ; continue());
<STRING>"\\""\\" => (addToBuffer "\\"; continue());
<STRING>"\\""\"" => (addToBuffer "\""; continue());
<STRING>"\\^". => (strFromCtrl(yypos, yytext); continue());
<STRING>"\\"{digit}{digit}{digit} => (strFromAscii(yypos, yytext); continue());
<STRING>("\\n" | "\\t" | " " | "\\f" | [^"\\"]) => (addToBuffer yytext; continue());
<STRING>{digit}+ => (addToBuffer yytext; continue());
<STRING>. => (ErrorMsg.error yypos ("Illegal use of \\ character."); continue());
<IGNORESEQ>"\\" => (YYBEGIN STRING; continue());
<IGNORESEQ>(\n) => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<IGNORESEQ>{nonprintable} => (continue());
<IGNORESEQ>. => (ErrorMsg.error yypos ("illegal use of printable character from IGNORESEQ"); continue());
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
