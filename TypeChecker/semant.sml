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
     (* val unitEntry = VarEntry({ty=Types.UNIT})
      val t12 = Symbol.enter(t11, Symbol.symbol("unit"), unitEntry) *)
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
    
  fun getTyOfVarEntry(Env.VarEntry({ty=t})) = t
  | getTyOfVarEntry(Env.FunEntry(_)) = Types.NIL
  
  fun getTy(tenv, sym, pos) = 
    let
      val s = Symbol.look(tenv, sym)
    in
      case s of SOME(x:Types.ty) => valOf s
      | NONE => (
        ErrorMsg.error pos "Undeclared type.";
        Types.NIL
      )
    end
    
  
  fun getArrayTy(tenv, sym, pos) =
    let val arrayTy = getTy(tenv, sym, pos)
    in
    case arrayTy of Types.NIL => (
      ErrorMsg.error pos "GG arraytype not found";
      Types.NIL
    )
    | Types.ARRAY(typ, un) => typ
    | (_) => (
      ErrorMsg.error pos "GG wrong type, not arraytype";
      Types.NIL
    )
    end
  
  
  
  (* return venv with params added *)
  fun addFunctionParamsVenv(venv,tenv,params:{name:Symbol.symbol,escape: bool ref, typ: Symbol.symbol, pos:Absyn.pos} list) =
    let fun foldFn (x, venv) = 
            case x of
              {name:Symbol.symbol,escape: bool ref, typ: Symbol.symbol, pos:Absyn.pos} =>
                let 
                  val paramType = getTypeOrNil(tenv,typ)
                in
                  case paramType of
                    Types.NIL  => (ErrorMsg.error pos "Unknown parameter type for function"; venv)
                  | _ => Symbol.enter(venv,name,Env.VarEntry({ty=paramType}))
                end
    in
      foldl foldFn venv params
    end
              
  (* Params: name = function name; params = (symbol*type) list; resultVal = symbol*pos 
     Returns: function entry to venv *)
  fun parseHeaders (name, params:{name: Symbol.symbol, escape: bool ref, 
		  typ: Symbol.symbol, pos: Absyn.pos} list) =
        (let
          val params_sym = map (fn x => #typ x) params
          val paramsTyList = 
            (* transTy on #1 resultVal *)
            let fun funcFoldrArg(sym, result) =
              let
                val foundType = getTypeOrNil(tenv, sym)
              in
                if foundType = Types.NIL
                then (ErrorMsg.error 0 "Unknown type in params list"; Types.NIL :: result)
                else
                  foundType :: result
              end
            in 
              foldr funcFoldrArg [] params_sym
            end
        in 
          paramsTyList
        end)
  
  fun venvWithFunctionHeaders fundeclist = 
  let 
    fun parseHeadersWithReturnType(x, venv) =
        case x of {name: Absyn.symbol, params , result: (Symbol.symbol * Absyn.pos) option, body: Absyn.exp, pos: Absyn.pos} =>
          (case result of SOME(symbol,pos) => (
            (* first pass put function headers into venv table *)
            let 
              val paramsList = parseHeaders (name, params)
              val resultTy = if getTypeOrNil(tenv, symbol) = Types.NIL 
                             then (ErrorMsg.error pos "return type not found in tenv"; Types.NIL)
                             else getTypeOrNil(tenv, symbol)
              val funentry = Env.FunEntry({formals=paramsList, result=resultTy})
            in
              Symbol.enter(venv, name, funentry)
            end
            (* this helper is called if isSome result, else return type is known as UNIT and is put in table*)
          )
          | NONE => (
            let 
              val paramsList = parseHeaders (name, params)
              val funentry = Env.FunEntry({formals=paramsList,result=Types.UNIT})
            in
              print("storing function with type unit as result");
              Symbol.enter(venv, name, funentry)
            end
          ))
    in
            foldr parseHeadersWithReturnType venv fundeclist
    end 
    
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


      and transExp(venv, tenv, exp): {exp:unit, ty:Types.ty} = (
        case exp of Absyn.IntExp(_) => (
          {exp=(), ty=Types.INT}
        )
          | Absyn.StringExp(_) => {exp=(),ty=Types.STRING}
          | Absyn.OpExp(_) => transOpExp(venv, tenv, exp)
          | Absyn.ArrayExp({typ=t, size=s, init=i, pos=p}) => (
              (* check size is int *)
                let val sizeTyp = #ty (transExp(venv,tenv,s))
                in  
                    case sizeTyp of 
                      Types.INT => (
                        (* check init same type as t *)
                          let 
                            val initTyp = (#ty (transExp(venv,tenv,i)))
                            val tTyp = getArrayTy(tenv, t, p)
                            val arrayTyp = Types.ARRAY(initTyp, ref ())
                          in
                            if initTyp = tTyp then (
                             
                              {exp=(), ty=arrayTyp}
                             
                            ) else (
                              ErrorMsg.error p "Type mismatch in arrayexp.";
                              {exp=(), ty=Types.NIL}
                            )
                            end
                        )
                      |(_) => (
                        (* array size is not int *)
                        ErrorMsg.error 0 "array size must be an int";
                        {exp=(), ty=Types.NIL}  
                      )
              end
            )
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
            
            | Absyn.SubscriptVar(var, exp, pos) => (
              (* First construct a VarExp a[4][4]*)
              let 
                val varExp = Absyn.VarExp(var)
                val varExpType = (#ty (transExp(venv, tenv, varExp)));
              in  (
                (* check type of subscript.var *)
                
                (* check if its an array type*)
                case varExpType of 
                (Types.ARRAY(typ, unique)) => 
                  (* check if exp is int type *)
                  let 
                    val expType = (#ty (transExp(venv, tenv, exp))) 
                  in
                    if expType = Types.INT then {exp=(),ty=typ} else (ErrorMsg.error pos "Array index must be integer";  {exp=(), ty = Types.NIL})
                  end
                | (_) => (ErrorMsg.error 0 "unmatched exp";
                         {exp=(), ty=Types.NIL}
                         )
                )
              end
              )
            )
          | Absyn.LetExp({decs, body, pos}) => 
            let 
              fun first  (a, _) = a
              fun second (_, b) = b
              val res = foldl (fn (dec, env) => transDec(#venv env, #tenv env,
              dec)) {venv=venv, tenv=tenv} decs
            in
              (* remember to push (venv, tenv) onto stack *)
              print("in letexp\n");
              transExp(#venv res,#tenv res, body)
              (** remember to pop from stack **)
            end
          | Absyn.SeqExp(xs) => let 
            fun first  (a, _) = a
            fun second (_, b) = b
            val res: {exp:unit, ty:Types.ty} = foldl (fn (x, y) => transExp(venv, tenv, first x)) {exp=(), ty=Types.NIL} xs
            in
              print("in seqexp\n");
              res
            end
          | Absyn.IfExp({test: Absyn.exp, then': Absyn.exp, else': Absyn.exp option, pos: Absyn.pos}) =>
            (* get the type of test, must be int *)
            if (#ty (transExp(venv, tenv, test))) <> Types.INT 
            then  
            (
            print("in ifexp\n");
            ErrorMsg.error pos "test of if else does not evaluate to int";
            {exp=(), ty=Types.NIL}
            )
            else (
              let 
                val thenTyp = transExp(venv, tenv, then')
              in
                print("in ifexp\n");
                if isSome else'
                then (
                  if (#ty (transExp(venv, tenv, valOf else')) <> #ty thenTyp) then ( 
                  ErrorMsg.error pos "branches have non-matching types" ;
                  {exp=(), ty=Types.NIL}
                  ) else (
                    thenTyp
                  )
                )
                else (
                  (* check that then' is of UNIT type*)
                if (#ty thenTyp) <> Types.UNIT then ( ErrorMsg.error pos "if-then statements must return unit."; {exp=(), ty=Types.NIL})
                else {exp=(), ty=Types.UNIT}
                )
              end
            )
          (*
          | Absyn.WhileExp({test: Absyn.exp, body: Absyn.exp, pos: Absyn.pos}) =>
            (* check test is of type int*)
            (let 
              val testType = #ty (transExp(venv, tenv, test))
              val lol = print("in whileExp\n")
            in
              (
              if testType = Types.INT 
              then
                (
                  let 
                    val bodyType = #ty (transExp(venv, tenv, body))   
                  in
                    (if bodyType <> Types.UNIT 
                    then
                      (
                        ErrorMsg.error pos "While expression has non-unit body type\n";
                        {exp=(), ty=Types.NIL}
                      )
                    else {exp=(), ty=Types.UNIT})
                  end
                )
              else
                (
                  ErrorMsg.error pos "While expression test condition has non-int type\n";
                  {exp=(),ty=Types.NIL}
                )
              )
            end)
            *)
            
          (* | Absyn.ForExp({var: Symbol.symbol, escape: bool ref, lo: Absyn.exp, hi: Absyn.exp, body: Absyn.exp, pos: Absyn.pos}) =>
            (* Remember to call enterScope here !!*)
            (* First, check exp1 is Types.INT*)
            ( let val loType = #ty (transExp(venv, tenv, lo))
              in
                (if loType <> Types.INT
                 then (
                  ErrorMsg.error pos "ForExp lower bound is not int type";
                  {exp=(), ty=Types.NIL}
                  )
                 else (
                  (* Check type of hi *)
                    let 
                       val hiType = #ty (transExp(venv, tenv, hi))
                    in  
                      (if hiType <> Types.INT
                       then (
                        ErrorMsg.error pos "ForExp upper bound is not int type";
                        {exp=(), ty=Types.NIL}
                       )
                       else (
                          (* Save var in venv *)
                          (* Rmb to enter scope *)
                          let 
                              val venv = Symbol.enter(venv, var, VarEntry({ty=Types.INT}))
                          in
                             (let val bodyType = #ty (transExp(venv, tenv, body))
                              in
                              (  
                                if bodyType <> Types.UNIT 
                                then (
                                  ErrorMsg.error pos "ForExp body not of UNIT type";
                                  {exp=(), ty=Types.NIL}
                                  )
                                else (
                                  {exp=(), ty = Types.UNIT}  
                                )
                              )
                              end)
                          end
                        )
                      )
                    end
                  )
                )
            end
            ) *)
          
          |_ => (
            ErrorMsg.error 0 "unmatched exp";
            {exp=(), ty=Types.NIL}
          )
        )
  
      (* Just handle non-recursive tydecs first *)
      and transDec(venv, tenv, dec): {venv:Env.enventry Symbol.table, tenv:Types.ty Symbol.table} =
        (* case dec of Absyn.TypeDec([{name, ty,pos}]) => (* change to match lists of all lengths for non-recursive case*)
          let
            val tenv = Symbol.enter(tenv, name, transTy(tenv, ty))
          in
            (*inEnv(tenv, Symbol.symbol("intarray"));*)
            print("inserting!!!");
            {venv=venv, tenv=tenv}
          end 
          
          *** ILLEGAL ***
          type a = b
          type b = a
          
          *** LEGAL ***
          type intlist = {hd:int, tl:intlist}
          
          type tree = {key:int, children:treelist}
          type treelist = {hd:tree, tl:treelist}
          
          *)
          
          case dec of Absyn.TypeDec(list) => {venv=venv, tenv=tenv}
              (* TO-DO
              case list of 
                  [] => 
                  [{name=name,ty=ty,pos=pos}::l] =>  
              *)
        | Absyn.VarDec({name, escape, typ, init, pos})	=> (* how to handle escape? what is it even??? *)
  	      (* if isSome typ then need to do type checking of init against typ, else just save the type of init for var name in venv*)
  	      if isSome typ then
            (
            let val expectedType = getTypeOrNil(tenv, #1 (valOf typ))
  	        in
  	          if expectedType = Types.NIL 
  	          then (
  	            ErrorMsg.error pos "Used Nil as a type.";
  	            {venv=venv, tenv=tenv}
  	          ) else (
    	          let val actualType = (#ty (transExp(venv, tenv, init)))
        	          val newVarEntry = Env.VarEntry({ty=actualType})
        	      in (
      	            (* begin of case *)
      	            case (actualType, expectedType) of (Types.ARRAY(sym, reff),Types.ARRAY(sym2, reff2)) => (
      	                  if sym = sym2 then (
            	              let 
            	                  val venv = Symbol.enter(venv, name, newVarEntry) 
            	              in 
            	                  (*inEnv(venv, Symbol.symbol("a"));*)
              	                {venv=venv, tenv=tenv}
              	            end
          	              ) else (
        	                  ErrorMsg.error pos "Declared type does not match exp type";
        	                  {venv=venv, tenv=tenv}
          	              ) 
      	                 ) 
      	            | (x, y) => (
      	                  if actualType = expectedType 
      	                  then 
        	                    let 
          	                      val venv = Symbol.enter(venv, name, newVarEntry) 
          	                  in 
        	                        (*inEnv(venv, Symbol.symbol("a"));*)
          	                      {venv=venv, tenv=tenv}
            	                end
            	            else  (
      	                      ErrorMsg.error pos "Var not found in venv.";
      	                      {venv=venv, tenv=tenv}
      	                  )
      	             ) 
      	             (* end of case *)
        	          
        	          ) (* end in *)
              end
  	         ) (* end else *)

  	         end
      	     
      	     ) else ( 
              (* typ is NONE, so nothing to check, just put actualType of init in table *)
              let val actualType = (#ty (transExp(venv, tenv, init)))
                  val newVarEntry = Env.VarEntry({ty=actualType})
              in
                  {venv = Symbol.enter(venv, name, newVarEntry), tenv = tenv}
              end
            )  
          
          (* TODO add fundec *)
        | Absyn.FunctionDec(fundeclist) =>
          (* add function headers to venv *)
          ( 
          (* foldl with transExp on body; returns boolean *)
          (let 
            val venv = venvWithFunctionHeaders(fundeclist)
            fun foldFn (fundec, bool) = 
            case bool of
              false => false
            | true =>
              (case fundec of 
                {name: Absyn.symbol, params:{name:Symbol.symbol,escape: bool ref, typ: Symbol.symbol, pos:Absyn.pos} list , result: (Symbol.symbol * Absyn.pos) option, body: Absyn.exp, pos: Absyn.pos} =>
                  let 
                    (* add parameters to venv *)
                    val venv = addFunctionParamsVenv(venv,tenv,params)
                    val actualRetType = #ty (transExp(venv, tenv, body))
                    val expectedReturnType = 
                      let 
                        (* look up function's return type in venv *)
                        val expectedReturnTypeOpt = Symbol.look(venv, name)
                      in
                        case expectedReturnTypeOpt of 
                          SOME(Env.FunEntry({formals=formals,result=result})) => result
                          | NONE => (ErrorMsg.error 0 "undefined function\n"; Types.NIL)
                      end
                  in
                    if expectedReturnType = actualRetType
                    then true
                    else (ErrorMsg.error pos "function return type mismatch\n";false)
                  end
              )
          in
            if (foldl foldFn true fundeclist) then {venv=venv, tenv=tenv} else (ErrorMsg.error 0 "fundecs do not type check\n"; {venv=venv, tenv=tenv})
          end))
          
 (*           | (_) => ( 
              ErrorMsg.error 0 "unmatched dec";
              {venv=venv, tenv=tenv}
            )
  *)        
          
         
    
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
        val res = 
        let 
          val lookup = Symbol.look(tenv, symbol)
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
