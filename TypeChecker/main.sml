structure Main : sig val tycheck : string -> unit  end =
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
    in TextIO.closeIn file;
    Semant.transProg(absyn)
end handle LrParser.ParseError => raise ErrorMsg.Error
    end
