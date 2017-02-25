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
      val printFunEntry = FunEntry({formals=[Types.STRING], result = Types.UNIT})
      val flushFunEntry = FunEntry({formals=[], result=Types.UNIT})
      val getcharFunEntry = FunEntry({formals = [], result = Types.STRING})
      val ordFunEntry = FunEntry({formals = [Types.STRING], result = Types.INT}) 
      val sizeFunEntry = FunEntry({formals=[Types.STRING], result=Types.INT})
      val substringFunEntry = FunEntry({formals=[Types.STRING, Types.INT, Types.INT], result = Types.STRING})
      val concatFunEntry = FunEntry({formals=[Types.STRING, Types.STRING], result=Types.STRING})
      val notFunEntry = FunEntry({formals = [Types.INT], result = Types.INT})
      val exitFunEntry = FunEntry({formals=[Types.INT], result=Types.UNIT})
      val t1 = Symbol.enter(emptyTable, Symbol.symbol("print"), printFunEntry);
      val t2 = Symbol.enter(t1, Symbol.symbol("flush"), flushFunEntry);
      val t3 = Symbol.enter(t2, Symbol.symbol("getchar"), getcharFunEntry);
      val t4 = Symbol.enter(t3, Symbol.symbol("ord"), ordFunEntry);
      val t5 = Symbol.enter(t4, Symbol.symbol("chr"), sizeFunEntry);
      val t6 = Symbol.enter(t5, Symbol.symbol("size"), substringFunEntry);
      val t7 = Symbol.enter(t6, Symbol.symbol("substring"), substringFunEntry);
      val t8 = Symbol.enter(t7, Symbol.symbol("concat"), concatFunEntry);
      val t9 = Symbol.enter(t8, Symbol.symbol("not"), notFunEntry);
      val t10 = Symbol.enter(t9, Symbol.symbol("exit"), exitFunEntry);

    in 
      t10
    end
  val base_tenv = initTypes()
  val base_venv = initValues()
    end
    (****** JOBS DONE *****)


structure Semant = 
struct
  open Absyn
  type venv = Env.enventry Symbol.table
  type tenv = ty Symbol.table
  type expty = {exp: unit, ty: Types.ty}
  val venv = Env.base_venv
  val tenv = Env.base_tenv
  (*transVar: venv * tenv * Absyn.var -> expty*)
  (*transExp: venv * tenv * Absyn.exp -> expty*)
  (*transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tevn}*)
  (*transTy: 	     tenv * Absyn.ty -> Types.ty*)
  (*transProg: Absyn.exp -> unit*)
  val stack = Array.array(100, Symbol.empty)
  val stackTop = 0


  fun transOpExp(venv, tenv, exp) = 
    case exp of 
         (OpExp({left,PlusOp,right}) | OpExp({left,MinusOp,right}) | OpExp({left,TimesOp,right}) | OpExp({left,DivideOp,right})) => 
         let 
           val {exp=_, ty=tyleft} = transExp(venv,tenv,left)
           val {exp=_, ty=tyright} = transExp(venv,tenv,right)
    in
      case tyleft of Types.INT => ()
         | _ => error pos "integer required";
         case tyright of Types.INT => ()
            | _ => error pos "integer required";
            {exp=(),ty=Types.INT}
end


            | (OpExp({left, GeOp, right}) | OpExp({left, LeOp, right}) | OpExp({left, GtOp, right}) | OpExp({left, LtOp, right})) =>
                let 
                  val {exp=_, ty=tyleft} = transExp(venv,tenv,left)
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
       | LetExp({decs, body, pos}) => 
           let 
             val res = foldl (fn (dec, env) => transDec(#1 env, #2 env, dec)) (venv, tenv) decs
                in
                  (* remember to push (venv, tenv) onto stack *)
                  transExp(#1 res, #2 res, body);
                  (** remember to pop from stack **)
                =end
       | SeqExp(xs) => let 
         val res = foldl (fn (x, res) => transExp(venv, tenv, #1 x)) {exp=(), ty=Types.NIL} xs
           in
             {exp=(),ty=(#2 res)}
           end
       | _ => error pos "gg"




       (* Just handle non-recursive tydecs first *)
  fun transDec(venv, tenv, dec) =
    case dec of TypeDec([{name, ty}]) =>
      {venv = venv, tenv = Symbol.enter(tenv, name, transTy(tenv, ty))}
       | _ => error pos "gg"
                       end


  fun transProg(e) =
    transExp(venv,tenv, e)
    print("jobs done!")
