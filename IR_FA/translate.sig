signature TRANSLATE =
sig
  type level
  type access
  type exp 
  
  val outermost : level
  val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
  val formals : level -> access list
  val allocLocal: level -> bool -> access
  val levelToString : level -> string
  val simpleVar : access * level -> exp
  val getResult: unit -> MipsFrame.frag list
  
  val stringExp : Absyn.exp -> exp
  val strcmp : exp * exp * Absyn.oper * level -> exp 
  val nilExp : unit -> exp 
  val intExp : Absyn.exp -> exp
  val opExp: exp * exp * Absyn.oper -> exp 
  val assignExp : exp * exp -> exp 
  val subscriptVar: exp * exp -> exp 
  val fieldVar : exp * int -> exp  (* fieldOffset as of in sorted field list *)
  val seqExp : exp list -> exp 
  val recordExp : (Symbol.symbol * exp) list -> exp 
  val arrayExp : exp * exp * level -> exp
  val callExp : level * level * Temp.label * exp list -> exp
  val transIf: {test:exp, then':exp, else': exp option, pos:Absyn.pos} -> exp
  val letExp : exp list * exp -> exp
  val funDec : level * Temp.label * exp -> unit 
  val varDecAlloc : access * exp -> exp 
  val transWhile: exp * exp * Temp.label -> exp
  val transFor: access * exp * exp * exp -> exp
  val transBreak: Temp.label -> exp
  val refCompare: exp * exp -> exp 
  
  val transError: unit -> exp
  val unNx: exp -> Tree.stm  (* TODO: temp *)
  
end
