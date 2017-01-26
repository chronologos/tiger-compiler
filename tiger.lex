type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

  val strBuffer = ref "";
  
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
  fun incrNest() = (commNest := !commNest + 1)
  fun decrNest() = let val newNest = (!commNest - 1) in commNest := newNest; !commNest end

%% 
  digits=[0-9]+;
  ws=(" " | "\t")+;				    
  %s STRING IGNORESEQ COMMENT;					   
%%
\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
","	=> (Tokens.COMMA(yypos,yypos+1));
var  	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL>{ws} => (print "Skipping whitespace\n"; continue());
<INITIAL>{digits}=> (Tokens.INT(valOf (Int.fromString yytext), yypos, yypos+size yytext)); 
<INITIAL>"\""=> (YYBEGIN STRING;print "String starting\n"; continue());
<STRING>"\""=> (YYBEGIN INITIAL;print "String ending\n"; Tokens.STRING(!strBuffer, yypos - 1 - clearBuffer(), yypos + 1));
<STRING>"\\"(\n | \t | " " | \f)+ => (YYBEGIN IGNORESEQ; print "Entering IGNORESEQ state\n"; continue());
<STRING>"\\""\\" => (print "Printing literal backslash character.\n"; addToBuffer "\\"; continue());
<STRING>("\\n" | "\\t" | " " | "\\f" | [^"\\"]) => (addToBuffer yytext; continue());
<STRING>{digits} => (print "Printing integer literal within string\n"; addToBuffer yytext; continue());
<STRING>. => (ErrorMsg.error yypos ("Illegal use of \\ character."); continue());
<IGNORESEQ>"\\" => (YYBEGIN STRING; print "Returning to STRING state from IGNORESEQ state\n"; continue());
<IGNORESEQ>. => (print "STAYING IN IGNORESEQ\n"; continue());
<INITIAL>"/*" => (print "Entering COMMENT\n"; YYBEGIN COMMENT; incrNest(); continue());
<COMMENT> "/*" => (print "Incrementing comment nestedness.\n"; incrNest();continue());
<COMMENT>"*/" => (print "Uncomment detected.\n"; if decrNest() = 0 then(YYBEGIN(INITIAL);print "Returning to INITIAL from COMMENT\n") else(print("Still in COMMENT, nesting: " ^ Int.toString(!commNest) ^ "\n")); continue());
<COMMENT>. => (print "Ignoring comment\n"; continue());
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

