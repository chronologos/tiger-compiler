structure Env :> ENV =
struct
  type access = int
  (*type ty = Types.ty*)
  datatype enventry = VarEntry of {ty: Types.ty}
                    | FunEntry of {formals: Types.ty list, result: Types.ty}


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
      val nilEntry = VarEntry({ty=Types.NIL})
      val t1 = Symbol.enter(emptyTable, Symbol.symbol("print"), printFunEntry)
      val t2 = Symbol.enter(t1, Symbol.symbol("flush"), flushFunEntry)
      val t3 = Symbol.enter(t2, Symbol.symbol("getchar"), getcharFunEntry)
      val t4 = Symbol.enter(t3, Symbol.symbol("ord"), ordFunEntry)
      val t5 = Symbol.enter(t4, Symbol.symbol("chr"), sizeFunEntry)
      val t6 = Symbol.enter(t5, Symbol.symbol("size"), substringFunEntry)
      val t7 = Symbol.enter(t6, Symbol.symbol("substring"), substringFunEntry)
      val t8 = Symbol.enter(t7, Symbol.symbol("concat"), concatFunEntry)
      val t9 = Symbol.enter(t8, Symbol.symbol("not"), notFunEntry)
      val t10 = Symbol.enter(t9, Symbol.symbol("exit"), exitFunEntry)
      val t11 = Symbol.enter(t10, Symbol.symbol("nil"), nilEntry)
    in 
      t11
    end
  val base_tenv = initTypes()
  val base_venv = initValues()
    end
    (****** JOBS DONE *****)


structure Semant = 
struct
  (* open Absyn *)
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table
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

  fun getTypeOrNil(table, sym) =
    let 
      val resOpt = Symbol.look(table, sym)
    in
      if isSome resOpt then valOf resOpt else Types.NIL
    end
    
  fun inEnv(env, sym) =
    if isSome (Symbol.look(env, sym)) then print("inside\n") else print("not inside\n")
    
  fun transProg(e) =
    let 
      fun transOpExp(venv, tenv, exp) = 
        case exp of 
          (Absyn.OpExp({left,oper=Absyn.PlusOp,right,pos}) |
          Absyn.OpExp({left,oper=Absyn.MinusOp,right,pos}) |
          Absyn.OpExp({left,oper=Absyn.TimesOp,right,pos}) |
          Absyn.OpExp({left,oper=Absyn.DivideOp,right,pos})) => 
            let 
              val {exp=_, ty=tyleft} = transExp(venv,tenv,left)
              val {exp=_, ty=tyright} = transExp(venv,tenv,right)
            in
              case tyleft of Types.INT => ()
              | _ => ErrorMsg.error 0 "integer required";
              case tyright of Types.INT => ()
              | _ => ErrorMsg.error 0 "integer required";
              {exp=(),ty=Types.INT}
            end


        | (Absyn.OpExp({left, oper=Absyn.GeOp, right,pos}) | Absyn.OpExp({left,
        oper=Absyn.LeOp,
        right,pos}) | Absyn.OpExp({left, oper=Absyn.GtOp, right,pos}) |
        Absyn.OpExp({left,
        oper=Absyn.LtOp, right,pos})) =>
          let 
            val {exp=_, ty=tyleft} = transExp(venv,tenv,left)
            val {exp=_, ty=tyright} = transExp(venv,tenv,right)
          in
            case (tyleft, tyright) of ((Types.INT, Types.INT) | (Types.STRING, Types.STRING)) => ()
            | (_,_) => ErrorMsg.error 0 "both operands must be either int or string";
            {exp=(),ty=Types.INT}
          end


        |(Absyn.OpExp({left, oper=Absyn.EqOp, right,pos}) | Absyn.OpExp({left,
          oper=Absyn.NeqOp, right,pos})) =>
          let 
            val {exp=_, ty=tyleft} = transExp(venv,tenv,left)
            val {exp=_, ty=tyright} = transExp(venv,tenv,right)
          in
            case (tyleft, tyright) of ((Types.STRING, Types.STRING)|
            (Types.INT, Types.INT)) => {exp=(), ty=Types.INT} 
            | (Types.RECORD(_ , x),Types.RECORD(_, y))  => if x = y then
              {exp=(), ty=Types.INT} else (
                ErrorMsg.error 0 "both operands must be of the same type";
                {exp=(), ty=tyleft}
              )
            | (Types.ARRAY(_ , x), Types.ARRAY(_,y))=> if x = y then {
              exp=(), ty=Types.INT} else (
              ErrorMsg.error pos "both operands must be of the same type";
              {exp=(), ty=Types.INT}
             )
            | ((Types.NIL, Types.RECORD(_, x)) | (Types.RECORD(_, x), Types.NIL))
             => {exp=(), ty=Types.INT}
            | (_, _) =>  (
              ErrorMsg.error 0 "both operands must be of the same type";
              {exp=(), ty=Types.INT} (*TODO*)
            )
          end
        | (_) => {exp=(),ty = Types.INT}


      and transExp(venv, tenv, exp): {exp:unit, ty:Types.ty} = 
        case exp of Absyn.IntExp(_) => {exp=(), ty=Types.INT}
          | Absyn.StringExp(_) => {exp=(),ty=Types.STRING}
          | Absyn.OpExp(_) => transOpExp(venv, tenv, exp)
          | Absyn.VarExp(somevar) => 
            ( case somevar of Absyn.SimpleVar(sym,pos) => 
              let 
                  val somety = Symbol.look(venv, sym)
              in 
                  case somety of
                    SOME(Env.FunEntry(_)) => (
                    ErrorMsg.error pos "Var not found in venv.";
                    {exp=(),ty=Types.NIL}
                    ) 
                  | SOME(Env.VarEntry({ty=ty})) => (
                    {exp=(), ty=ty})
                  | NONE => (
                    ErrorMsg.error pos "Var not found in venv.";
                    {exp=(),ty=Types.NIL}
                  )
                  
              end
            )
          | Absyn.LetExp({decs, body, pos}) => 
            let 
              fun first  (a, _) = a
              fun second (_, b) = b
              val res = foldl (fn (dec, env) => transDec(#venv env, #tenv env,
              dec)) {venv=venv, tenv=tenv} decs
              in
                (* remember to push (venv, tenv) onto stack *)
                
                transExp(#venv res,#tenv res, body)
                (** remember to pop from stack **)
              end
          | Absyn.SeqExp(xs) => let 
            fun first  (a, _) = a
            fun second (_, b) = b
            val res: {exp:unit, ty:Types.ty} = foldl (fn (x, y) => transExp(venv, tenv, first x)) {exp=(), ty=Types.NIL} xs
            in
              res
            end
          | _ => (
            ErrorMsg.error 0 "unmatched exp";
            {exp=(), ty=Types.NIL}
            (* TODO *)
          )

      (* Just handle non-recursive tydecs first *)
      and transDec(venv, tenv, dec): {venv:Env.enventry Symbol.table, tenv:Types.ty Symbol.table} =
        case dec of Absyn.TypeDec([{name, ty,pos}]) => let
          val tenv = Symbol.enter(tenv, name, transTy(tenv, ty))
          in
            {venv=venv, tenv=tenv}
          end
        | Absyn.VarDec({name, escape, typ, init, pos})	=> (* how to handle escape? what is it even??? *)
  	      (* if isSome typ then need to do type checking of init against typ, else just save the type of init for var name in venv*)
  	      if isSome typ then
            (let val expectedType = getTypeOrNil(tenv, #1 (valOf typ))
  	        in
  	         if expectedType = Types.NIL then (
  	           ErrorMsg.error pos "Used Nil as a type.";
  	           {venv=venv, tenv=tenv}
  	         ) else (
    	         let val actualType = (#ty (transExp(venv, tenv, init)))
        	     val newVarEntry = Env.VarEntry({ty=actualType})
        	        in
        	          print("entering sym into venv\n");
        	          if expectedType = actualType 
        	          then (
        	            let 
        	            val venv = Symbol.enter(venv, name, newVarEntry) in
        	           
        	             
          	            {venv=venv, tenv=tenv}
          	          end
        	          )
          	        else(
          	          ErrorMsg.error pos "Declared type does not match exp type";
          	          {venv=venv, tenv=tenv}
        	          )
                  end
  	         )
      	     
            end)
            else ( 
              (* typ is NONE, so nothing to check, just put actualType of init in table *)
              let val actualType = (#ty (transExp(venv, tenv, init)))
              val newVarEntry = Env.VarEntry({ty=actualType})
              in
                
                {venv = Symbol.enter(venv, name, newVarEntry), tenv = tenv}
              end
            )  
          
          (* TODO add fundec *)
        | _ => ( 
          ErrorMsg.error 0 "unmatched dec";
          {venv=venv, tenv=tenv}
        )
         
    
  and transTy(tenv, somety) = 

    case somety of Absyn.NameTy(symbol, pos) => 
        let 
          val s = Symbol.look(tenv,symbol)
        in
            if isSome(s) 
            then valOf s 
            else Types.NIL
        end
    | Absyn.RecordTy(fieldlist) => 
      let
        val resList = map (fn x => let 
                                      val lookup = Symbol.look(tenv,#typ x)
                                   in
                                      if isSome lookup 
                                      then (#name x, valOf lookup)
                                      else (#name x, Types.NIL)
                                   end) fieldlist
      in
        (*TODO verify unit ref can be local*)
        Types.RECORD((resList, ref ()))
      end
    | Absyn.ArrayTy(symbol, pos) => 
      let
        val res = let val lookup = Symbol.look(tenv, symbol)
      in
        if isSome lookup 
        then valOf lookup 
        else Types.NIL
      end
      in
        Types.ARRAY(res, ref ())
      end

      in
        transExp(venv,tenv, e);
        print "jobs done!"
    end
                         end
