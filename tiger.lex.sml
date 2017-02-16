structure Mlex  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
STRING | COMMENT | IGNORESEQ | INITIAL
    structure UserDeclarations = 
      struct

type pos = int
type lexresult = Tokens.token

exception LexError of int

fun raiseLexError(pos:int) =
  raise LexError(pos)

fun lexErrorWithPrint(str:string, pos: int) =
  let val dummy = print("[ABOUT TO THROW AN ERROR GG ]"^str)
  in
    raiseLexError(pos)
  end

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



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TYPE(yypos,yypos+4)))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(yypos,yypos+1)))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.VAR(yypos,yypos+3)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ASSIGN(yypos,yypos+2)))
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.INT(Option.valOf(Int.fromString(yytext)), yypos, yypos+String.size(yytext) ))
      end
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.FUNCTION(yypos, yypos + size yytext))
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.BREAK(yypos, yypos + size yytext))
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.OF(yypos, yypos + size yytext))
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.END(yypos, yypos + size yytext))
      end
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.IN(yypos, yypos + size yytext))
      end
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.NIL(yypos, yypos + size yytext))
      end
fun yyAction14 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LET(yypos, yypos + size yytext))
      end
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.DO(yypos, yypos + size yytext))
      end
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.TO(yypos, yypos + size yytext))
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.FOR(yypos, yypos + size yytext))
      end
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.IF(yypos,yypos+2)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ARRAY(yypos,yypos+5)))
fun yyAction20 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.WHILE(yypos, yypos + size yytext))
      end
fun yyAction21 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ELSE(yypos, yypos + size yytext))
      end
fun yyAction22 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.THEN(yypos, yypos + size yytext))
      end
fun yyAction23 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.ID(yytext, yypos, (yypos + String.size(yytext)) ))
      end
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COMMENT; print "Entering COMMENT\n"; inComment := true; incrNest(); continue()))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "Incrementing comment nestedness.\n"; inComment := true; incrNest();continue()))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue()))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "Uncomment detected.\n"; if decrNest() = 0 then(YYBEGIN(INITIAL); inComment := false; print "Returning to INITIAL from COMMENT\n") else(print("Still in COMMENT, nesting: " ^ Int.toString(!commNest) ^ "\n")); continue()))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN STRING; inString := true; print "String starting\n"; continue()))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL;print "String ending\n"; inString := false; Tokens.STRING(!strBuffer, yypos - 1 - clearBuffer(), yypos + 1)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN IGNORESEQ; print "Entering IGNORESEQ state\n"; continue()))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "Printing literal backslash character.\n"; addToBuffer "\\"; continue()))
fun yyAction33 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addToBuffer yytext; continue())
      end
fun yyAction34 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (print "Printing integer literal within string\n"; addToBuffer yytext; continue())
      end
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (ErrorMsg.error yypos ("Illegal use of \\ character."); continue()))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN STRING; print "Returning to STRING state from IGNORESEQ state\n"; continue()))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lexErrorWithPrint("nonprintable character between slashes in string",yypos)))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "Printable character received from IGNORESEQ"; ErrorMsg.error yypos ("illegal use of printable character from IGNORESEQ"); continue()))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NEQ(yypos,yypos+2)))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OR(yypos,yypos+2)))
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.AND(yypos,yypos+2)))
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GE(yypos,yypos+2)))
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LE(yypos,yypos+2)))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GT(yypos,yypos+1)))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LT(yypos,yypos+1)))
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQ(yypos,yypos+1)))
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DIVIDE(yypos,yypos+1)))
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TIMES(yypos,yypos+1)))
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MINUS(yypos,yypos+1)))
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PLUS(yypos,yypos+1)))
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DOT(yypos,yypos+1)))
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACE(yypos,yypos+1)))
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACE(yypos,yypos+1)))
fun yyAction54 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACK(yypos,yypos+1)))
fun yyAction55 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACK(yypos,yypos+1)))
fun yyAction56 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPAREN(yypos,yypos+1)))
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LPAREN(yypos,yypos+1)))
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMICOLON(yypos,yypos+1)))
fun yyAction59 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COLON(yypos,yypos+1)))
fun yyAction60 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(yypos,yypos+1)))
fun yyAction61 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (ErrorMsg.error yypos ("illegal character " ^ yytext); continue())
      end
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction23(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp = #"`"
              then yyAction23(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction20(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp = #"`"
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ64(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ63(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ62(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"h"
              then yyQ61(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"h"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction5(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction5(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp = #"`"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ66(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ65(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction1(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction1(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction1(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"`"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ71(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ70(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction16(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction16(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction16(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction16(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp = #"`"
              then yyAction16(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction22(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction22(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction22(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction22(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
            else if inp = #"`"
              then yyAction22(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                  else yyAction22(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ73(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ72(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction23(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                      else yyAction23(strm, yyNO_MATCH)
                else if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"_"
                  then if inp <= #"Z"
                      then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                      else yyAction23(strm, yyNO_MATCH)
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"i"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"i"
                  then if inp = #"h"
                      then yyQ67(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"o"
                  then yyQ68(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp = #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"z"
              then if inp = #"y"
                  then yyQ69(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction10(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction10(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction10(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp = #"`"
              then yyAction10(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ74(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction13(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction13(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction13(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction13(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp = #"`"
              then yyAction13(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ76(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ75(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction14(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction14(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction14(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction14(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp = #"`"
              then yyAction14(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyAction14(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ78(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ77(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction12(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction12(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction12(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction12(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp = #"`"
              then yyAction12(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction18(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction18(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction18(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction18(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp = #"`"
              then yyAction18(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction23(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction23(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                      else yyAction23(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ80(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"f"
                  then yyQ79(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = #"`"
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ88(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ87(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ86(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ85(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"c"
              then yyQ84(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"c"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ83(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction17(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction17(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = #"`"
              then yyAction17(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ89(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction23(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction23(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                      else yyAction23(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ82(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"o"
                  then yyQ81(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction11(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction11(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction11(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp = #"`"
              then yyAction11(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"d"
              then yyQ92(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"d"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction21(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction21(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction21(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp = #"`"
              then yyAction21(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ94(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ93(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction23(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction23(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                      else yyAction23(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ91(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"l"
                  then yyQ90(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction15(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction15(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction15(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction15(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp = #"`"
              then yyAction15(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ95(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction9(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction9(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction9(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp = #"`"
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"k"
              then yyQ99(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"k"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ97 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ98(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ97(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ96(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction19(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction19(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp = #"`"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"y"
              then yyQ103(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"y"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp = #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ102(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ101(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ100(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction55(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction23(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp = #"`"
              then yyAction23(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ104(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
              else yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ106(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ105(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ107 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ107(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
              else yyAction59(strm, yyNO_MATCH)
      (* end case *))
fun yyQ108 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ108(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp = #"`"
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ108(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp = #"`"
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ60(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ60(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ109 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ109(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction57(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction57(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ110 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ110(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ110(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ110(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ110(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction61(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction61(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\\"
              then yyQ23(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"\\"
              then if inp = #"+"
                  then yyQ31(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"+"
                  then if inp = #"\""
                      then yyQ26(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                    else if inp < #"\""
                      then if inp = #"\v"
                          then yyQ23(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                        else if inp < #"\v"
                          then if inp = #"\t"
                              then yyQ24(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                            else if inp = #"\n"
                              then yyQ25(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                              else yyQ23(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                        else if inp = #" "
                          then yyQ24(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                          else yyQ23(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                    else if inp = #"("
                      then yyQ28(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                    else if inp < #"("
                      then if inp = #"&"
                          then yyQ27(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                          else yyQ23(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                    else if inp = #")"
                      then yyQ29(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                      else yyQ30(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #";"
                  then yyQ38(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #";"
                  then if inp = #"/"
                      then yyQ35(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                    else if inp < #"/"
                      then if inp = #"-"
                          then yyQ33(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                        else if inp = #","
                          then yyQ32(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                          else yyQ34(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                    else if inp = #":"
                      then yyQ37(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                      else yyQ36(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"?"
                  then yyQ23(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"?"
                  then if inp = #"="
                      then yyQ40(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                    else if inp = #"<"
                      then yyQ39(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                      else yyQ41(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ42(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"A"
                  then yyQ23(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"["
                  then yyQ43(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyQ42(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp = #"l"
              then yyQ51(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"c"
                  then yyQ42(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"c"
                  then if inp = #"`"
                      then yyQ23(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                    else if inp < #"`"
                      then if inp = #"^"
                          then yyQ23(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                        else if inp = #"]"
                          then yyQ44(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                          else yyQ42(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                    else if inp = #"a"
                      then yyQ45(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                      else yyQ46(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"g"
                  then yyQ42(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"g"
                  then if inp = #"e"
                      then yyQ48(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                    else if inp = #"d"
                      then yyQ47(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                      else yyQ49(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"i"
                  then yyQ50(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyQ42(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp = #"v"
              then yyQ55(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"v"
              then if inp = #"p"
                  then yyQ42(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"p"
                  then if inp = #"n"
                      then yyQ52(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                    else if inp = #"m"
                      then yyQ42(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                      else yyQ53(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"t"
                  then yyQ54(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyQ42(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp = #"|"
              then yyQ58(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"|"
              then if inp = #"x"
                  then yyQ42(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"x"
                  then yyQ56(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"{"
                  then yyQ57(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyQ42(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp = #"}"
              then yyQ59(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyQ23(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #" "
              then yyQ21(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\t"
                  then yyQ21(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= #"\n"
                  then yyQ21(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ21(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #" "
              then yyQ21(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\t"
                  then yyQ21(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= #"\n"
                  then yyQ21(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ21(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"!"
              then yyQ19(strm', lastMatch)
            else if inp < #"!"
              then if inp = #"\n"
                  then yyQ21(strm', lastMatch)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ20(strm', lastMatch)
                      else yyQ19(strm', lastMatch)
                else if inp = #" "
                  then yyQ20(strm', lastMatch)
                  else yyQ19(strm', lastMatch)
            else if inp = #"]"
              then yyQ19(strm', lastMatch)
            else if inp < #"]"
              then if inp = #"\\"
                  then yyQ22(strm', lastMatch)
                  else yyQ19(strm', lastMatch)
            else if inp = #"f"
              then yyQ20(strm', lastMatch)
              else yyQ19(strm', lastMatch)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ17(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyQ18(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ15(strm', lastMatch)
            else if inp < #"*"
              then if inp = #"\n"
                  then yyQ14(strm', lastMatch)
                  else yyQ13(strm', lastMatch)
            else if inp = #"/"
              then yyQ16(strm', lastMatch)
              else yyQ13(strm', lastMatch)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #" "
              then yyQ9(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\t"
                  then yyQ9(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= #"\n"
                  then yyQ9(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ9(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #" "
              then yyQ9(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\t"
                  then yyQ9(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= #"\n"
                  then yyQ9(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ9(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"]"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #"]"
              then if inp = #" "
                  then yyQ9(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                else if inp < #" "
                  then if inp = #"\t"
                      then yyQ9(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                    else if inp < #"\t"
                      then yyAction35(strm, yyNO_MATCH)
                    else if inp <= #"\n"
                      then yyQ9(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                      else yyAction35(strm, yyNO_MATCH)
                else if inp = #"\\"
                  then yyQ10(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ5(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"f"
                  then yyQ11(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ5(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ12(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
            else if inp < #"0"
              then yyAction34(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ12(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ12(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"0"
              then yyAction33(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ12(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ4(strm', lastMatch)
            else if inp < #"#"
              then if inp = #"\v"
                  then yyQ4(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ5(strm', lastMatch)
                      else yyQ4(strm', lastMatch)
                else if inp = #"\""
                  then yyQ6(strm', lastMatch)
                  else yyQ4(strm', lastMatch)
            else if inp = #":"
              then yyQ4(strm', lastMatch)
            else if inp < #":"
              then if inp <= #"/"
                  then yyQ4(strm', lastMatch)
                  else yyQ7(strm', lastMatch)
            else if inp = #"\\"
              then yyQ8(strm', lastMatch)
              else yyQ4(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of STRING => yyQ0(!(yystrm), yyNO_MATCH)
    | COMMENT => yyQ1(!(yystrm), yyNO_MATCH)
    | IGNORESEQ => yyQ2(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ3(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
