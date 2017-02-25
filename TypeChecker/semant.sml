structure Env :> ENV =
struct
type access = int
type ty = Types.ty
datatype enventry = VarEntry of {ty: ty}
                  | FunEntry of {formals: ty list, result: ty}


(***** types ******)
(* initialize all keywords? *)
	
fun initTypes() = 
  let val emptyTable = Symbol.empty
      val int_init = Symbol.enter(emptyTable, Symbol.symbol("int"), Types.INT);
      val string_init = Symbol.enter(int_init,Symbol.symbol("string"),Types.STRING)
      val initialized = Symbol.enter(string_init, Symbol.symbol("nil"), Types.NIL)
  in
      initialized
  end


(**** functions and vars *******)
fun initValues() =
  let val emptyTable = Symbol.empty
      val printFuncEntry = FuncEntry({formals=[Types.STRING], result = Types.UNIT})
      val flushFuncEntry = FuncEntry({formals=[], result=Types.UNIT})
      val getcharFuncEntry = FuncEntry({formals = [], result = Types.STRING})
      val ordFuncEntry = FuncEntry({formals = [Types.string], result = Types.INT}) 
      val sizeFuncEntry = FuncEntry({formals=[Types.STRING], result=Types.INT})
      val substringFuncEntry = FuncEntry({formals=[Types.STRING, Types.INT, Types.INT], result = Types.STRING})
      val concatFuncEntry = FuncEntry({formals=[Types.STRING, Types.STRING], result=Types.STRING})
      val notFuncEntry = FuncEntry({formals = [Types.INT], result = Types.INT})
      val exitFuncEntry = FuncEntry({formals=[Types.INT], result=Types.UNIT})
	val t1 = Symbol.enter(emptyTable, Symbol.symbol("print"), printFuncEntry);
val t2 = Symbol.enter(t1, Symbol.symbol("flush"), flushFuncEntry);
val t3 = Symbol.enter(t2, Symbol.symbol("getchar"), getcharFuncEntry);
	val t4 = Symbol.enter(t3, Symbol.symbol("ord"), ordFuncEntry);
val t5 = Symbol.enter(t4, Symbol.symbol("chr"), sizeFuncEntry);
val t6 = Symbol.enter(t5, Symbol.symbol("size"), substringFuncEntry);
val t7 = Symbol.enter(t6, Symbol.symbol("substring"), substringFuncEntry);
val t8 = Symbol.enter(t7, Symbol.symbol("concat"), concatFuncEntry);
val t9 = Symbol.enter(t8, Symbol.symbol("not"), notFuncEntry);
val t10 = Symbol.enter(t9, Symbol.symbol("exit"), exitFuncEntry);

in 
  t10
end
val base_tenv = initTypes()
val base_venv = initValues()

(****** JOBS DONE *****)


structure Semant = 
struct
	open Absyn
	type venv = Env.enventry Symbol.table
	type tenv = ty Symbol.table
	type expty = {exp: unit, ty: Types.ty}
        val venv = Env.base_venv
        val tenv = Env.base_tenv
	transVar: venv * tenv * Absyn.var -> expty
	transExp: venv * tenv * Absyn.exp -> expty
	transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tevn}
	transTy: 	     tenv * Absyn.ty -> Types.ty
	transProg: Absyn.exp -> unit
      val stack = Array.array(100, Symbol.empty)
	val stackTop = 0

fun transProg(e) =
  transExp(venv,tenv, e)
  print("jobs done!")

fun transOpExp(venv, tenv, exp) = 
	case exp of 
		(OpExp({left,PlusOp,right}) | OpExp({left,MinusOp,right}) | OpExp({left,TimesOp,right}) | OpExp({left,DivideOp,right})) => 
let val {exp=_, ty=tyleft} = transExp(venv,tenv,left)
	    		val {exp=_, ty=tyright} = transExp(venv,tenv,right)
		in
    	    case tyleft of Types.INT => ()
   			| _ => error pos "integer required";
    	    case tyright of Types.INT => ()
   			| _ => error pos "integer required";
    	    {exp=(),ty=Types.INT}
end


| (OpExp({left, GeOp, right}) | OpExp({left, LeOp, right}) | OpExp({left, GtOp, right}) | OpExp({left, LtOp, right})) =>
	let val {exp=_, ty=tyleft} = transExp(venv,tenv,left)
	    		val {exp=_, ty=tyright} = transExp(venv,tenv,right)
		in
    	    case (tyleft, tyright) of ((Types.INT, Types.INT) | (Types.STRING, Types.STRING)) => ()
                | (_,_) => error pos "both operands must be either int or string";
    	    {exp=(),ty=Types.INT}
end


|(OpExp({left, EqOp, right}) | OpExp({left, NeqOp, right})) =>
	case (tyleft, tyright) of ((Types.STRING, Types.STRING)| (Types.INT, Types.INT)) => ()
| (Types.RECORD(_ , x),Types.RECORD(_, y)) => if x = y then () else error pos "both operands must be of the same type"
| (Types.ARRAY(_ , x), Types.ARRAY(_,y))=> if x = y then () else error pos "both operands must be of the same type"
| (_, _) => error pos "both operands must be of the same type"


		

fun transExp(venv, tenv, exp) = 
	case exp of IntExp => {exp=(), ty=Types.INT}
	| StringExp(_) => {exp=(),ty=Types.STRING}
| OpExp(_) => transOpExp(venv, tenv, exp)
| LetExp({decs, body, pos}) => let () in
    (* remember to push (venv, tenv) onto stack *)
    (val res = foldl (fn (dec, env) => transDec(#1 env, #2 env, dec)) (venv, tenv) decs
		     transExp(#1 res, #2 res, body);)

(** remember to pop from stack **)
| SeqExp(xs) => let () in
	(val res = foldl (fn (x, res) => transExp(venv, tenv, #1 x)) {exp=(), ty=Types.NIL} xs
{exp=(),ty=#2 res};)

	


(* Just handle non-recursive tydecs first *)
fun transDec(venv, tenv, dec) =
	case dec of TypeDec([{name, ty}]) =>
		{venv = venv, tenv = Symbol.enter(tenv, name, transTy(tenv, ty))}
