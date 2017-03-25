structure Main : sig val tycheck : string -> MipsFrame.frag list end =
struct
  structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
  structure Lex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
  structure TigerP = Join(structure ParserData = TigerLrVals.ParserData
  structure Lex=Lex
  structure LrParser = LrParser)
  fun tycheck filename =
    let val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
      val file = TextIO.openIn filename
      fun get _ = TextIO.input file
      fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
      val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
      val (absyn, _) = TigerP.parse(30,lexer,parseerror,())
    in
      TextIO.closeIn file;
      (*
      print("before\n");
      PrintAbsyn.print(TextIO.stdOut, absyn);
      print("after\n");
      PrintAbsyn.print(TextIO.stdOut, absyn);
      *)
      FindEscape.findEscape(absyn);
      let 
        val res = Semant.transProg(absyn)
        fun foldFn (x,result) = case x of MipsFrame.STRING(_) => result
                                                 | MipsFrame.PROC({body=body,frame=_}) => (
                                                      print("\nFUNCTION:\n\n");
                                                      Printtree.printtree (TextIO.stdOut, body); 
                                                      print("\n-----------\n");
                                                      result
                                                 )
      in
      ( 
        foldl foldFn [] res;
        res
      )
      end
    end handle LrParser.ParseError => raise ErrorMsg.Error
    end
