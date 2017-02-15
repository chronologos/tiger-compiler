functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Absyn
structure S = Symbol
type symbol = S.symbol
val symbol = S.symbol

type field = {name: symbol, escape: bool ref, typ: symbol, pos: A.pos}
type fundec = {name: symbol, params: field list, result: (symbol * A.pos) option, body: A.exp, pos: A.pos}
type pos = A.pos
type exp = A.exp


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\176\000\002\000\176\000\003\000\176\000\004\000\176\000\
\\005\000\176\000\007\000\176\000\009\000\176\000\011\000\176\000\
\\013\000\176\000\015\000\027\000\016\000\026\000\017\000\025\000\
\\018\000\024\000\029\000\176\000\030\000\176\000\031\000\176\000\
\\036\000\176\000\037\000\176\000\038\000\176\000\041\000\176\000\
\\042\000\176\000\043\000\176\000\044\000\176\000\000\000\
\\001\000\001\000\177\000\002\000\177\000\003\000\177\000\004\000\177\000\
\\005\000\177\000\007\000\177\000\009\000\177\000\011\000\177\000\
\\013\000\177\000\015\000\027\000\016\000\026\000\017\000\025\000\
\\018\000\024\000\029\000\177\000\030\000\177\000\031\000\177\000\
\\036\000\177\000\037\000\177\000\038\000\177\000\041\000\177\000\
\\042\000\177\000\043\000\177\000\044\000\177\000\000\000\
\\001\000\001\000\178\000\002\000\178\000\003\000\178\000\004\000\178\000\
\\005\000\178\000\007\000\178\000\009\000\178\000\011\000\178\000\
\\013\000\178\000\015\000\027\000\016\000\026\000\017\000\025\000\
\\018\000\024\000\029\000\178\000\030\000\178\000\031\000\178\000\
\\036\000\178\000\037\000\178\000\038\000\178\000\041\000\178\000\
\\042\000\178\000\043\000\178\000\044\000\178\000\000\000\
\\001\000\001\000\179\000\002\000\179\000\003\000\179\000\004\000\179\000\
\\005\000\179\000\007\000\179\000\009\000\179\000\011\000\179\000\
\\013\000\179\000\015\000\027\000\016\000\026\000\017\000\025\000\
\\018\000\024\000\029\000\179\000\030\000\179\000\031\000\179\000\
\\036\000\179\000\037\000\179\000\038\000\179\000\041\000\179\000\
\\042\000\179\000\043\000\179\000\044\000\179\000\000\000\
\\001\000\001\000\180\000\002\000\180\000\003\000\180\000\004\000\180\000\
\\005\000\180\000\007\000\180\000\009\000\180\000\011\000\180\000\
\\013\000\180\000\015\000\027\000\016\000\026\000\017\000\025\000\
\\018\000\024\000\029\000\180\000\030\000\180\000\031\000\180\000\
\\036\000\180\000\037\000\180\000\038\000\180\000\041\000\180\000\
\\042\000\180\000\043\000\180\000\044\000\180\000\000\000\
\\001\000\001\000\181\000\002\000\181\000\003\000\181\000\004\000\181\000\
\\005\000\181\000\007\000\181\000\009\000\181\000\011\000\181\000\
\\013\000\181\000\015\000\027\000\016\000\026\000\017\000\025\000\
\\018\000\024\000\029\000\181\000\030\000\181\000\031\000\181\000\
\\036\000\181\000\037\000\181\000\038\000\181\000\041\000\181\000\
\\042\000\181\000\043\000\181\000\044\000\181\000\000\000\
\\001\000\001\000\028\000\015\000\027\000\016\000\026\000\017\000\025\000\
\\018\000\024\000\019\000\023\000\020\000\022\000\021\000\021\000\
\\022\000\020\000\023\000\019\000\024\000\018\000\000\000\
\\001\000\002\000\172\000\003\000\172\000\004\000\172\000\005\000\172\000\
\\009\000\172\000\015\000\172\000\016\000\172\000\017\000\172\000\
\\018\000\172\000\019\000\172\000\020\000\172\000\021\000\172\000\
\\022\000\172\000\023\000\172\000\024\000\172\000\029\000\172\000\
\\036\000\172\000\041\000\172\000\000\000\
\\001\000\002\000\014\000\003\000\013\000\004\000\012\000\016\000\011\000\
\\029\000\010\000\036\000\009\000\041\000\008\000\000\000\
\\001\000\002\000\044\000\000\000\
\\001\000\002\000\060\000\000\000\
\\001\000\002\000\061\000\000\000\
\\001\000\002\000\062\000\000\000\
\\001\000\002\000\065\000\013\000\064\000\000\000\
\\001\000\002\000\088\000\012\000\087\000\028\000\086\000\000\000\
\\001\000\002\000\090\000\000\000\
\\001\000\002\000\093\000\000\000\
\\001\000\002\000\110\000\000\000\
\\001\000\002\000\117\000\000\000\
\\001\000\002\000\119\000\000\000\
\\001\000\002\000\122\000\000\000\
\\001\000\006\000\075\000\027\000\074\000\000\000\
\\001\000\006\000\106\000\000\000\
\\001\000\006\000\116\000\019\000\115\000\000\000\
\\001\000\008\000\076\000\000\000\
\\001\000\009\000\079\000\000\000\
\\001\000\009\000\105\000\000\000\
\\001\000\011\000\069\000\015\000\027\000\016\000\026\000\017\000\025\000\
\\018\000\024\000\019\000\023\000\020\000\022\000\021\000\021\000\
\\022\000\020\000\023\000\019\000\024\000\018\000\000\000\
\\001\000\013\000\111\000\000\000\
\\001\000\013\000\118\000\000\000\
\\001\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\030\000\063\000\000\000\
\\001\000\019\000\072\000\000\000\
\\001\000\019\000\078\000\000\000\
\\001\000\019\000\123\000\000\000\
\\001\000\019\000\124\000\000\000\
\\001\000\027\000\102\000\000\000\
\\001\000\037\000\059\000\000\000\
\\001\000\038\000\083\000\000\000\
\\001\000\039\000\100\000\000\000\
\\129\000\000\000\
\\130\000\042\000\038\000\043\000\037\000\044\000\036\000\000\000\
\\131\000\044\000\036\000\000\000\
\\132\000\000\000\
\\133\000\042\000\038\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\002\000\093\000\000\000\
\\143\000\000\000\
\\144\000\005\000\104\000\000\000\
\\145\000\000\000\
\\146\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\000\000\
\\147\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\000\000\
\\151\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\000\000\
\\152\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\000\000\
\\153\000\008\000\042\000\012\000\041\000\000\000\
\\154\000\000\000\
\\155\000\000\000\
\\161\000\000\000\
\\162\000\007\000\084\000\015\000\027\000\016\000\026\000\017\000\025\000\
\\018\000\024\000\019\000\023\000\020\000\022\000\021\000\021\000\
\\022\000\020\000\023\000\019\000\024\000\018\000\000\000\
\\163\000\002\000\014\000\003\000\013\000\004\000\012\000\016\000\011\000\
\\029\000\010\000\036\000\009\000\041\000\008\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\010\000\017\000\014\000\016\000\027\000\015\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\017\000\025\000\018\000\024\000\000\000\
\\174\000\017\000\025\000\018\000\024\000\000\000\
\\175\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\000\000\
\\186\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\031\000\095\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\005\000\109\000\015\000\027\000\016\000\026\000\017\000\025\000\
\\018\000\024\000\019\000\023\000\020\000\022\000\021\000\021\000\
\\022\000\020\000\023\000\019\000\024\000\018\000\000\000\
\\190\000\000\000\
\\191\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\000\000\
\\192\000\002\000\014\000\003\000\013\000\004\000\012\000\005\000\068\000\
\\015\000\027\000\016\000\081\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\029\000\010\000\036\000\009\000\
\\041\000\008\000\000\000\
\\192\000\002\000\014\000\003\000\013\000\004\000\012\000\005\000\068\000\
\\016\000\011\000\029\000\010\000\036\000\009\000\041\000\008\000\000\000\
\\193\000\000\000\
\\194\000\000\000\
\\195\000\000\000\
\"
val actionRowNumbers =
"\009\000\077\000\083\000\076\000\
\\074\000\007\000\071\000\041\000\
\\009\000\009\000\072\000\070\000\
\\064\000\009\000\010\000\009\000\
\\009\000\009\000\009\000\009\000\
\\009\000\009\000\009\000\009\000\
\\009\000\009\000\096\000\044\000\
\\060\000\043\000\042\000\046\000\
\\041\000\037\000\011\000\012\000\
\\013\000\031\000\078\000\014\000\
\\093\000\091\000\065\000\028\000\
\\004\000\003\000\005\000\006\000\
\\002\000\001\000\082\000\081\000\
\\079\000\080\000\059\000\045\000\
\\040\000\069\000\032\000\022\000\
\\025\000\009\000\087\000\033\000\
\\026\000\092\000\009\000\066\000\
\\038\000\068\000\015\000\056\000\
\\009\000\016\000\053\000\086\000\
\\009\000\075\000\094\000\009\000\
\\092\000\073\000\069\000\047\000\
\\039\000\053\000\048\000\057\000\
\\036\000\055\000\027\000\023\000\
\\084\000\009\000\089\000\008\000\
\\095\000\067\000\018\000\029\000\
\\009\000\052\000\017\000\024\000\
\\019\000\085\000\030\000\020\000\
\\050\000\049\000\058\000\055\000\
\\061\000\009\000\021\000\051\000\
\\088\000\034\000\054\000\062\000\
\\035\000\009\000\009\000\089\000\
\\063\000\090\000\000\000"
val gotoT =
"\
\\001\000\005\000\002\000\126\000\016\000\004\000\024\000\003\000\
\\027\000\002\000\029\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\033\000\004\000\032\000\005\000\031\000\006\000\030\000\
\\007\000\029\000\008\000\028\000\009\000\027\000\000\000\
\\001\000\037\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\001\000\038\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\041\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\000\000\
\\001\000\043\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\001\000\044\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\001\000\045\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\001\000\046\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\001\000\047\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\001\000\048\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\001\000\049\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\001\000\050\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\001\000\051\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\001\000\052\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\001\000\053\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\000\000\
\\008\000\054\000\000\000\
\\000\000\
\\000\000\
\\005\000\055\000\000\000\
\\000\000\
\\003\000\056\000\004\000\032\000\005\000\031\000\006\000\030\000\
\\007\000\029\000\008\000\028\000\009\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\065\000\016\000\004\000\024\000\003\000\026\000\064\000\
\\027\000\002\000\029\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\069\000\016\000\004\000\020\000\068\000\024\000\003\000\
\\027\000\002\000\029\000\001\000\000\000\
\\000\000\
\\014\000\071\000\000\000\
\\000\000\
\\001\000\075\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\065\000\016\000\004\000\024\000\003\000\026\000\078\000\
\\027\000\002\000\029\000\001\000\000\000\
\\001\000\080\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\083\000\000\000\
\\000\000\
\\001\000\087\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\000\000\
\\011\000\090\000\012\000\089\000\000\000\
\\028\000\092\000\000\000\
\\001\000\094\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\095\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\001\000\065\000\016\000\004\000\024\000\003\000\026\000\096\000\
\\027\000\002\000\029\000\001\000\000\000\
\\000\000\
\\001\000\069\000\016\000\004\000\020\000\097\000\024\000\003\000\
\\027\000\002\000\029\000\001\000\000\000\
\\000\000\
\\000\000\
\\011\000\099\000\012\000\089\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\101\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\105\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\025\000\106\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\110\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\000\000\
\\012\000\111\000\000\000\
\\015\000\112\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\118\000\000\000\
\\000\000\
\\001\000\119\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\123\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\001\000\124\000\016\000\004\000\024\000\003\000\027\000\002\000\
\\029\000\001\000\000\000\
\\025\000\125\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 127
val numrules = 67
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string) | ifExp of unit ->  (A.exp)
 | ifExpOpt of unit ->  (A.exp option) | assignExp of unit ->  (A.exp)
 | funCallExpList of unit ->  (A.exp list)
 | recordExpOpt of unit ->  ( ( symbol * exp * pos )  list)
 | recordExp of unit ->  (A.exp) | opExp of unit ->  (A.exp)
 | strExp of unit ->  (A.exp) | nilExp of unit ->  (A.exp)
 | letSeqExp of unit ->  ( ( A.exp * A.pos )  list)
 | seqexpOpt2 of unit ->  ( ( A.exp * A.pos )  list)
 | seqexpOpt of unit ->  ( ( A.exp * A.pos )  list)
 | seqexp of unit ->  (A.exp) | lvalue of unit ->  (A.var)
 | fundecOpts of unit ->  ({ body:A.exp,result: ( symbol * pos )  option } )
 | varDecOpt of unit ->  ({ typ: ( symbol * A.pos )  option,init:A.exp } )
 | tyfieldscommaOpt of unit ->  (field list)
 | tyfieldsOne of unit ->  (field) | tyfields of unit ->  (field list)
 | ty of unit ->  (A.ty) | fundecs of unit ->  (fundec list)
 | fundec of unit ->  (fundec)
 | vardec of unit ->  ({ name:symbol,escape:bool ref,typ: ( symbol * A.pos )  option,init:A.exp,pos:A.pos } )
 | tydecs of unit ->  ({ name:symbol,ty:A.ty,pos:A.pos }  list)
 | tydec of unit ->  ({ name:symbol,ty:A.ty,pos:A.pos } )
 | dec of unit ->  (A.dec) | decs of unit ->  (A.dec list)
 | program of unit ->  (A.exp) | exp of unit ->  (A.exp)
end
type svalue = MlyValue.svalue
type result = A.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 31) => true | (T 32) => true | (T 33) => true | (T 39) => true
 | (T 35) => true | (T 36) => true | (T 37) => true | (T 41) => true
 | (T 42) => true | (T 43) => true | (T 27) => true | (T 28) => true
 | (T 29) => true | (T 30) => true | (T 34) => true | (T 38) => true
 | (T 40) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 29))::
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "TIMES"
  | (T 17) => "DIVIDE"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LT"
  | (T 21) => "LE"
  | (T 22) => "GT"
  | (T 23) => "GE"
  | (T 24) => "AND"
  | (T 25) => "OR"
  | (T 26) => "ASSIGN"
  | (T 27) => "ARRAY"
  | (T 28) => "IF"
  | (T 29) => "THEN"
  | (T 30) => "ELSE"
  | (T 31) => "WHILE"
  | (T 32) => "FOR"
  | (T 33) => "TO"
  | (T 34) => "DO"
  | (T 35) => "LET"
  | (T 36) => "IN"
  | (T 37) => "END"
  | (T 38) => "OF"
  | (T 39) => "BREAK"
  | (T 40) => "NIL"
  | (T 41) => "FUNCTION"
  | (T 42) => "VAR"
  | (T 43) => "TYPE"
  | (T 44) => "UMINUS"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( 
MlyValue.dec dec1, dec1left, _)) :: rest671)) => let val  result = 
MlyValue.decs (fn _ => let val  (dec as dec1) = dec1 ()
 val  (decs as decs1) = decs1 ()
 in (dec :: decs)
end)
 in ( LrTable.NT 2, ( result, dec1left, decs1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.decs (fn _ => ([]))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.tydecs tydecs1, tydecs1left, tydecs1right))
 :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (
tydecs as tydecs1) = tydecs1 ()
 in (A.TypeDec(tydecs))
end)
 in ( LrTable.NT 3, ( result, tydecs1left, tydecs1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.vardec vardec1, vardec1left, vardec1right))
 :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (
vardec as vardec1) = vardec1 ()
 in (A.VarDec(vardec))
end)
 in ( LrTable.NT 3, ( result, vardec1left, vardec1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.fundecs fundecs1, fundecs1left, 
fundecs1right)) :: rest671)) => let val  result = MlyValue.dec (fn _
 => let val  (fundecs as fundecs1) = fundecs1 ()
 in (A.FunctionDec(fundecs))
end)
 in ( LrTable.NT 3, ( result, fundecs1left, fundecs1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.tydec tydec1, _, tydec1right)) :: ( _, ( 
MlyValue.tydecs tydecs1, tydecs1left, _)) :: rest671)) => let val  
result = MlyValue.tydecs (fn _ => let val  (tydecs as tydecs1) = 
tydecs1 ()
 val  (tydec as tydec1) = tydec1 ()
 in (tydec::tydecs)
end)
 in ( LrTable.NT 5, ( result, tydecs1left, tydec1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.tydec tydec1, tydec1left, tydec1right)) :: 
rest671)) => let val  result = MlyValue.tydecs (fn _ => let val  (
tydec as tydec1) = tydec1 ()
 in ([tydec])
end)
 in ( LrTable.NT 5, ( result, tydec1left, tydec1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ty ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) =>
 let val  result = MlyValue.tydec (fn _ => let val  (ID as ID1) = ID1
 ()
 val  (ty as ty1) = ty1 ()
 in ({name=S.symbol(ID), ty=ty, pos=0})
end)
 in ( LrTable.NT 4, ( result, TYPE1left, ty1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ty (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.NameTy(S.symbol(ID), 0))
end)
 in ( LrTable.NT 9, ( result, ID1left, ID1right), rest671)
end
|  ( 9, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.tyfields 
tyfields1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.ty (fn _ => let val  (tyfields as tyfields1) =
 tyfields1 ()
 in (A.RecordTy(tyfields))
end)
 in ( LrTable.NT 9, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, 
ARRAY1left, _)) :: rest671)) => let val  result = MlyValue.ty (fn _ =>
 let val  (ID as ID1) = ID1 ()
 in (A.ArrayTy(S.symbol(ID), 0))
end)
 in ( LrTable.NT 9, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.tyfieldsOne (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in (
{name = S.symbol(ID1), escape = ref false, typ = S.symbol(ID2), pos = 0}
)
end)
 in ( LrTable.NT 11, ( result, ID1left, ID2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.tyfieldscommaOpt tyfieldscommaOpt1, _, 
tyfieldscommaOpt1right)) :: ( _, ( MlyValue.tyfieldsOne tyfieldsOne1, 
tyfieldsOne1left, _)) :: rest671)) => let val  result = 
MlyValue.tyfields (fn _ => let val  (tyfieldsOne as tyfieldsOne1) = 
tyfieldsOne1 ()
 val  (tyfieldscommaOpt as tyfieldscommaOpt1) = tyfieldscommaOpt1 ()
 in ( tyfieldsOne :: tyfieldscommaOpt)
end)
 in ( LrTable.NT 10, ( result, tyfieldsOne1left, 
tyfieldscommaOpt1right), rest671)
end
|  ( 13, ( rest671)) => let val  result = MlyValue.tyfields (fn _ => (
[]))
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 14, ( ( _, ( MlyValue.tyfieldscommaOpt tyfieldscommaOpt1, _, 
tyfieldscommaOpt1right)) :: ( _, ( MlyValue.tyfieldsOne tyfieldsOne1,
 _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result =
 MlyValue.tyfieldscommaOpt (fn _ => let val  (tyfieldsOne as 
tyfieldsOne1) = tyfieldsOne1 ()
 val  (tyfieldscommaOpt as tyfieldscommaOpt1) = tyfieldscommaOpt1 ()
 in (tyfieldsOne :: tyfieldscommaOpt)
end)
 in ( LrTable.NT 12, ( result, COMMA1left, tyfieldscommaOpt1right), 
rest671)
end
|  ( 15, ( rest671)) => let val  result = MlyValue.tyfieldscommaOpt
 (fn _ => ([]))
 in ( LrTable.NT 12, ( result, defaultPos, defaultPos), rest671)
end
|  ( 16, ( ( _, ( MlyValue.varDecOpt varDecOpt1, _, varDecOpt1right))
 :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: 
rest671)) => let val  result = MlyValue.vardec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (varDecOpt as varDecOpt1) = varDecOpt1 ()
 in (
{name = S.symbol(ID), escape = ref false, typ = #typ varDecOpt, init = #init varDecOpt, pos = 0}
)
end)
 in ( LrTable.NT 6, ( result, VAR1left, varDecOpt1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
ASSIGN1left, _)) :: rest671)) => let val  result = MlyValue.varDecOpt
 (fn _ => let val  (exp as exp1) = exp1 ()
 in ({typ = NONE, init = exp})
end)
 in ( LrTable.NT 13, ( result, ASSIGN1left, exp1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, COLON1left, _)) :: rest671)) =>
 let val  result = MlyValue.varDecOpt (fn _ => let val  (ID as ID1) = 
ID1 ()
 val  (exp as exp1) = exp1 ()
 in ({typ = SOME(S.symbol(ID), 0), init = exp })
end)
 in ( LrTable.NT 13, ( result, COLON1left, exp1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.fundec fundec1, _, fundec1right)) :: ( _, (
 MlyValue.fundecs fundecs1, fundecs1left, _)) :: rest671)) => let val 
 result = MlyValue.fundecs (fn _ => let val  (fundecs as fundecs1) = 
fundecs1 ()
 val  (fundec as fundec1) = fundec1 ()
 in ( fundecs @ [fundec] )
end)
 in ( LrTable.NT 8, ( result, fundecs1left, fundec1right), rest671)

end
|  ( 20, ( ( _, ( MlyValue.fundec fundec1, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.fundecs (fn _ => let val 
 (fundec as fundec1) = fundec1 ()
 in ([fundec])
end)
 in ( LrTable.NT 8, ( result, fundec1left, fundec1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.fundecOpts fundecOpts1, _, fundecOpts1right
)) :: _ :: ( _, ( MlyValue.tyfields tyfields1, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671))
 => let val  result = MlyValue.fundec (fn _ => let val  (ID as ID1) = 
ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  (fundecOpts as fundecOpts1) = fundecOpts1 ()
 in (
{name= S.symbol(ID), params=tyfields, result = #result fundecOpts, body = #body fundecOpts, pos = 0}
)
end)
 in ( LrTable.NT 7, ( result, FUNCTION1left, fundecOpts1right), 
rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
EQ1left, _)) :: rest671)) => let val  result = MlyValue.fundecOpts (fn
 _ => let val  (exp as exp1) = exp1 ()
 in ({body = exp, result = NONE})
end)
 in ( LrTable.NT 14, ( result, EQ1left, exp1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, COLON1left, _)) :: rest671)) =>
 let val  result = MlyValue.fundecOpts (fn _ => let val  (ID as ID1) =
 ID1 ()
 val  (exp as exp1) = exp1 ()
 in ({body = exp, result = SOME(S.symbol(ID), 0)})
end)
 in ( LrTable.NT 14, ( result, COLON1left, exp1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.lvalue (fn _ => let val  (ID as ID1) = ID1
 ()
 in (A.SimpleVar(symbol(ID),0))
end)
 in ( LrTable.NT 15, ( result, ID1left, ID1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.lvalue lvalue1, lvalue1left, _)) :: rest671)) => let val  
result = MlyValue.lvalue (fn _ => let val  (lvalue as lvalue1) = 
lvalue1 ()
 val  (ID as ID1) = ID1 ()
 in (A.FieldVar(lvalue,symbol(ID),0))
end)
 in ( LrTable.NT 15, ( result, lvalue1left, ID1right), rest671)
end
|  ( 26, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( MlyValue.lvalue lvalue1, lvalue1left, _)) :: 
rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (
lvalue as lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in (A.SubscriptVar(lvalue,exp,0))
end)
 in ( LrTable.NT 15, ( result, lvalue1left, RBRACK1right), rest671)

end
|  ( 27, ( ( _, ( MlyValue.seqexpOpt seqexpOpt1, _, seqexpOpt1right))
 :: ( _, ( MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.seqexp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  (seqexpOpt as seqexpOpt1) = seqexpOpt1 ()
 in (A.SeqExp((exp1, 0) :: (exp2, 0) :: seqexpOpt))
end)
 in ( LrTable.NT 16, ( result, LPAREN1left, seqexpOpt1right), rest671)

end
|  ( 28, ( ( _, ( _, RPAREN1left, RPAREN1right)) :: rest671)) => let
 val  result = MlyValue.seqexpOpt (fn _ => ([]))
 in ( LrTable.NT 17, ( result, RPAREN1left, RPAREN1right), rest671)

end
|  ( 29, ( ( _, ( MlyValue.seqexpOpt seqexpOpt1, _, seqexpOpt1right))
 :: ( _, ( MlyValue.seqexpOpt2 seqexpOpt21, seqexpOpt21left, _)) :: 
rest671)) => let val  result = MlyValue.seqexpOpt (fn _ => let val  (
seqexpOpt2 as seqexpOpt21) = seqexpOpt21 ()
 val  (seqexpOpt as seqexpOpt1) = seqexpOpt1 ()
 in (seqexpOpt2 @ seqexpOpt)
end)
 in ( LrTable.NT 17, ( result, seqexpOpt21left, seqexpOpt1right), 
rest671)
end
|  ( 30, ( ( _, ( MlyValue.seqexpOpt2 seqexpOpt21, _, seqexpOpt21right
)) :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, SEMICOLON1left, _)
) :: rest671)) => let val  result = MlyValue.seqexpOpt2 (fn _ => let
 val  (exp as exp1) = exp1 ()
 val  (seqexpOpt2 as seqexpOpt21) = seqexpOpt21 ()
 in ((exp, 0) :: seqexpOpt2)
end)
 in ( LrTable.NT 18, ( result, SEMICOLON1left, seqexpOpt21right), 
rest671)
end
|  ( 31, ( rest671)) => let val  result = MlyValue.seqexpOpt2 (fn _ =>
 ([]))
 in ( LrTable.NT 18, ( result, defaultPos, defaultPos), rest671)
end
|  ( 32, ( ( _, ( MlyValue.letSeqExp letSeqExp1, _, letSeqExp1right))
 :: _ :: ( _, ( MlyValue.exp exp1, (expleft as exp1left), _)) :: 
rest671)) => let val  result = MlyValue.letSeqExp (fn _ => let val  (
exp as exp1) = exp1 ()
 val  (letSeqExp as letSeqExp1) = letSeqExp1 ()
 in ((exp, expleft) :: letSeqExp)
end)
 in ( LrTable.NT 19, ( result, exp1left, letSeqExp1right), rest671)

end
|  ( 33, ( ( _, ( MlyValue.exp exp1, (expleft as exp1left), exp1right)
) :: rest671)) => let val  result = MlyValue.letSeqExp (fn _ => let
 val  (exp as exp1) = exp1 ()
 in ([(exp, expleft)])
end)
 in ( LrTable.NT 19, ( result, exp1left, exp1right), rest671)
end
|  ( 34, ( rest671)) => let val  result = MlyValue.letSeqExp (fn _ =>
 ([]))
 in ( LrTable.NT 19, ( result, defaultPos, defaultPos), rest671)
end
|  ( 35, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (A.IntExp(INT))
end)
 in ( LrTable.NT 0, ( result, INT1left, INT1right), rest671)
end
|  ( 36, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => (A.NilExp))
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.STRING STRING1, (STRINGleft as STRING1left)
, STRING1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (STRING as STRING1) = STRING1 ()
 in (A.StringExp(STRING, STRINGleft))
end)
 in ( LrTable.NT 0, ( result, STRING1left, STRING1right), rest671)
end
|  ( 38, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.letSeqExp 
letSeqExp1, _, _)) :: _ :: ( _, ( MlyValue.decs decs1, _, _)) :: ( _, 
( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn
 _ => let val  (decs as decs1) = decs1 ()
 val  (letSeqExp as letSeqExp1) = letSeqExp1 ()
 in (A.LetExp({decs = decs, body = A.SeqExp(letSeqExp), pos = 0}))
end
)
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.lvalue lvalue1, lvalue1left, lvalue1right))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
lvalue as lvalue1) = lvalue1 ()
 in (A.VarExp(lvalue))
end)
 in ( LrTable.NT 0, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 40, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( 
MlyValue.funCallExpList funCallExpList1, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => let val  (ID as ID1) = ID1 ()
 val  (funCallExpList as funCallExpList1) = funCallExpList1 ()
 in (A.CallExp({func=symbol(ID), args=funCallExpList, pos=IDleft }))

end)
 in ( LrTable.NT 0, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.recordExp recordExp1, recordExp1left, 
recordExp1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (recordExp as recordExp1) = recordExp1 ()
 in (recordExp)
end)
 in ( LrTable.NT 0, ( result, recordExp1left, recordExp1right), 
rest671)
end
|  ( 42, ( ( _, ( MlyValue.ifExp ifExp1, ifExp1left, ifExp1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (ifExp
 as ifExp1) = ifExp1 ()
 in (ifExp)
end)
 in ( LrTable.NT 0, ( result, ifExp1left, ifExp1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, (
MINUSleft as MINUS1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (
A.OpExp({left=A.IntExp(0),oper=A.MinusOp,right=exp,pos=MINUSleft}))

end)
 in ( LrTable.NT 0, ( result, MINUS1left, exp1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
MINUSleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper= A.MinusOp, right=exp2,pos=MINUSleft}))

end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
PLUSleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper= A.PlusOp, right=exp2, pos=PLUSleft}))

end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
TIMESleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper= A.TimesOp, right=exp2,pos=TIMESleft}))

end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
EQleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper= A.EqOp, right=exp2,pos=EQleft}))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
NEQleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper= A.NeqOp, right=exp2,pos=NEQleft}))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
GTleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper= A.GtOp, right=exp2,pos=GTleft}))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
GEleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper= A.GeOp, right=exp2,pos=GEleft}))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
LEleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper= A.LeOp, right=exp2,pos=LEleft}))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
LTleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper= A.LtOp, right=exp2,pos=LTleft}))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( _, 
DIVIDEleft, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1, oper= A.DivideOp, right=exp2,pos=DIVIDEleft})
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.assignExp assignExp1, assignExp1left, 
assignExp1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (assignExp as assignExp1) = assignExp1 ()
 in (assignExp)
end)
 in ( LrTable.NT 0, ( result, assignExp1left, assignExp1right), 
rest671)
end
|  ( 55, ( ( _, ( MlyValue.ifExpOpt ifExpOpt1, _, ifExpOpt1right)) :: 
( _, ( MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  
result = MlyValue.ifExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  (ifExpOpt as ifExpOpt1) = ifExpOpt1 ()
 in (A.IfExp({test=exp1,then'=exp2,else'=ifExpOpt,pos=IFleft}))
end)
 in ( LrTable.NT 28, ( result, IF1left, ifExpOpt1right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
ELSE1left, _)) :: rest671)) => let val  result = MlyValue.ifExpOpt (fn
 _ => let val  (exp as exp1) = exp1 ()
 in (SOME(exp))
end)
 in ( LrTable.NT 27, ( result, ELSE1left, exp1right), rest671)
end
|  ( 57, ( rest671)) => let val  result = MlyValue.ifExpOpt (fn _ => (
NONE))
 in ( LrTable.NT 27, ( result, defaultPos, defaultPos), rest671)
end
|  ( 58, ( ( _, ( _, _, RBRACE1right)) :: _ :: ( _, ( MlyValue.ID ID1,
 (IDleft as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.recordExp (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.RecordExp({fields=[],typ=symbol(ID),pos=IDleft}))
end)
 in ( LrTable.NT 23, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 59, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.recordExpOpt
 recordExpOpt1, _, _)) :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _
, ( MlyValue.ID ID2, ID2left, _)) :: _ :: ( _, ( MlyValue.ID ID1, 
ID1left, _)) :: rest671)) => let val  result = MlyValue.recordExp (fn
 _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 val  (recordExpOpt as recordExpOpt1) = recordExpOpt1 ()
 in (
A.RecordExp({fields=(symbol(ID2),exp,ID2left)::recordExpOpt,typ=symbol(ID1),pos=ID1left})
)
end)
 in ( LrTable.NT 23, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 60, ( rest671)) => let val  result = MlyValue.recordExpOpt (fn _
 => ([]))
 in ( LrTable.NT 24, ( result, defaultPos, defaultPos), rest671)
end
|  ( 61, ( ( _, ( MlyValue.recordExpOpt recordExpOpt1, _, 
recordExpOpt1right)) :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, 
( MlyValue.ID ID1, IDleft, _)) :: ( _, ( _, COMMA1left, _)) :: rest671
)) => let val  result = MlyValue.recordExpOpt (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 val  (recordExpOpt as recordExpOpt1) = recordExpOpt1 ()
 in ((symbol(ID),exp,IDleft)::recordExpOpt)
end)
 in ( LrTable.NT 24, ( result, COMMA1left, recordExpOpt1right), 
rest671)
end
|  ( 62, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
ASSIGNleft, _)) :: ( _, ( MlyValue.lvalue lvalue1, lvalue1left, _)) ::
 rest671)) => let val  result = MlyValue.assignExp (fn _ => let val  (
lvalue as lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in (A.AssignExp({var=lvalue,exp=exp,pos=ASSIGNleft}))
end)
 in ( LrTable.NT 26, ( result, lvalue1left, exp1right), rest671)
end
|  ( 63, ( rest671)) => let val  result = MlyValue.funCallExpList (fn
 _ => ([]))
 in ( LrTable.NT 25, ( result, defaultPos, defaultPos), rest671)
end
|  ( 64, ( ( _, ( MlyValue.funCallExpList funCallExpList1, _, 
funCallExpList1right)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: 
rest671)) => let val  result = MlyValue.funCallExpList (fn _ => let
 val  (exp as exp1) = exp1 ()
 val  (funCallExpList as funCallExpList1) = funCallExpList1 ()
 in (exp :: funCallExpList)
end)
 in ( LrTable.NT 25, ( result, exp1left, funCallExpList1right), 
rest671)
end
|  ( 65, ( ( _, ( MlyValue.funCallExpList funCallExpList1, _, 
funCallExpList1right)) :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _
, COMMA1left, _)) :: rest671)) => let val  result = 
MlyValue.funCallExpList (fn _ => let val  (exp as exp1) = exp1 ()
 val  (funCallExpList as funCallExpList1) = funCallExpList1 ()
 in (exp :: funCallExpList)
end)
 in ( LrTable.NT 25, ( result, COMMA1left, funCallExpList1right), 
rest671)
end
|  ( 66, ( ( _, ( _, _, EOF1right)) :: ( _, ( MlyValue.exp exp1, 
exp1left, _)) :: rest671)) => let val  result = MlyValue.program (fn _
 => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 1, ( result, exp1left, EOF1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
end
end
