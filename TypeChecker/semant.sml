structure Env :> ENV =
struct
  type access = int
  (*type ty = Types.ty*)
  datatype enventry = VarEntry of {ty: Types.ty}
                    | FunEntry of {formals: Types.ty list, result: Types.ty}

  (***** types ******)
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

structure Semant = 
struct
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

  fun getTy(tenv, sym, pos) = 
    let
      val s = Symbol.look(tenv, sym)
    in
      case s of SOME(x:Types.ty) => valOf s
      | NONE => (
        ErrorMsg.error pos "Undeclared type.";
        Types.BOTTOM
      )
    end
    
  fun printDatatype(Types.RECORD(_)) = print("RECORD")
  | printDatatype(Types.INT) = print("INT")
  | printDatatype(Types.STRING) = print("STRING")
  | printDatatype(Types.UNIT) = print("UNIT")
  | printDatatype(Types.BOTTOM) = print("BOTTOM")
  | printDatatype(Types.ARRAY(_)) = print("ARRAY")
  | printDatatype(Types.NIL) = print("NIL")
  
  fun printGetFieldsOutput (out:(Symbol.symbol*Types.ty) list) = (
    map (fn x => (print(Symbol.name(#1 x)); print(":"); printDatatype(#2 x); print("\n"))) out;
    print("\n")
  )
  
  (* add record type symbols to someEnv and create unit ref *)
  fun unitRefFolder ({name=name, ty=Absyn.RecordTy(fieldList), pos=pos}, someEnv) =
    Symbol.enter(someEnv, name, ref ())
  | unitRefFolder (_, someEnv) =
    someEnv
    
  fun unitRefFolder ({name=name, ty=Absyn.RecordTy(fieldList), pos=pos}, someEnv) =
    Symbol.enter(someEnv, name, ref ())
  | unitRefFolder (_, someEnv) =
    someEnv
  
  (* return a table of symbol -> unit ref *)
  fun createUnitRef decList =
    let 
      val firstPassTable = Symbol.empty
    in
      foldl unitRefFolder firstPassTable decList
    end

 fun checkDupsFoldFunc(currentDec:{name: Symbol.symbol, ty: Absyn.ty, pos: Absyn.pos}, (listOfNames,err)) = 
  let 
    val found = List.find (fn(x) => x = (#name (currentDec))) listOfNames
  in  
    if err = 1 
    then (listOfNames,err)
    else (
      if isSome found 
      then (
        ErrorMsg.error 0 "Duplicate declaration in declist";
        (listOfNames, 1)
      )
      else(
        ((#name (currentDec)) :: listOfNames, 0)
      )
    )
  end
  
  fun tyEqualTo(ty1, ty2) = 
    case (ty1, ty2) of 
      (Types.RECORD(_), Types.RECORD(_)) => true
    | (Types.ARRAY(_), Types.ARRAY(_)) => true
    | (Types.INT, Types.INT) => true
    | (Types.STRING, Types.STRING) => true
    | (Types.NIL, Types.NIL) => true
    | (Types.UNIT, Types.UNIT) => true
    | (Types.BOTTOM, _) => true
    | (_, Types.BOTTOM) => true
    | (_, _) => false
    
  fun tyListEqualTo(tylist1:Types.ty list, tylist2:Types.ty list) = 
    if List.length(tylist1) <> List.length(tylist2) then false 
    else 
    (
      let 
        val zippedList = ListPair.zip(tylist1, tylist2) 
      in
        foldl (fn (nextTup, equalSoFar) => if  tyEqualTo((#1 nextTup),(#2 nextTup)) then equalSoFar else false) true zippedList
      end
    )

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
              case foundType of Types.NIL =>
                (ErrorMsg.error 0 "Unknown type in params list"; Types.BOTTOM :: result)
              | (_) => foundType :: result
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
              val resultTy = 
              let val typeGotten = getTypeOrNil(tenv, symbol)
              in
                case typeGotten of Types.NIL =>  (ErrorMsg.error pos "return type not found in tenv"; Types.NIL)
                | (_) => typeGotten
              end
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
   
  fun getTyOfName(declist : {name: Symbol.symbol, ty: Absyn.ty, pos: Absyn.pos} list, name) =
    let val foundOpt = List.find (fn(x)=>(#name x) = name) declist
    in 
      foundOpt
    end

  fun getSymFromFieldList(sym, fieldlist) =
    let fun getField(currentField:Symbol.symbol * Types.ty, typSoFar:Types.ty) = 
      if Symbol.name(#1 (currentField) ) = Symbol.name(sym) 
      then #2 (currentField)
      else typSoFar
    in
        foldl getField Types.BOTTOM fieldlist
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
            in (
              case tyleft of Types.INT => {exp=(),ty=Types.INT}
              | (_) => (ErrorMsg.error pos "integer required"; {exp=(),ty=Types.BOTTOM});
              case tyright of Types.INT => {exp=(),ty=Types.INT}
              | (_) => (ErrorMsg.error pos "integer required"; {exp=(),ty=Types.BOTTOM})
            )
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


      and transExp(venv:venv, tenv:tenv, exp): {exp:unit, ty:Types.ty} = (
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
                        val tTyp = getTy(tenv, t, p)
                      in
                        case tTyp of Types.ARRAY(array_func, _) =>
                          if tyEqualTo(initTyp, array_func()) then (
                            {exp=(), ty=tTyp}
                          ) else (
                            ErrorMsg.error p "Type mismatch in arrayexp.";
                            {exp=(), ty=Types.NIL}
                          )
                        | (_) => (ErrorMsg.error p "Tried to initialize array with non array type."; {exp=(), ty=Types.NIL})
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
                val varType = (#ty (transVar(venv, tenv, var)));
              in  (
                (* check type of subscript.var *)
                
                (* check if its an array type*)
                case varType of 
                  (Types.ARRAY(typ, unique)) => 
                  (* check if exp is int type *)
                  let 
                    val expType = (#ty (transExp(venv, tenv, exp))) 
                  in
                    if tyEqualTo(expType, Types.INT) then {exp=(),ty=typ()} else (ErrorMsg.error pos "Array index must be integer";  {exp=(), ty = Types.NIL})
                  end
                
                | (_) => (ErrorMsg.error pos "SubscriptVar on non array type \n";
                         {exp=(), ty=Types.NIL}
                         )
                )
              end
              )
           | Absyn.FieldVar(var,symbol,pos) =>
              (* var must be record type, get field list, check if symbol is in fieldlist, return type of symbol *)
              let val varTyp = #ty (transVar(venv, tenv, var))
              in
                case varTyp of 
                  Types.RECORD(get_fields_func, unit_ref) =>
                    let 
                      val fields_list = get_fields_func()
                      
                    (*  val typ = getSymFromFieldList(symbol, fields_list) *)
                        val typ_opt = List.find (fn x => Symbol.name(#1 (x)) = Symbol.name(symbol)) fields_list
                    in (
                      printGetFieldsOutput(fields_list);
                      case typ_opt of 
                        SOME(found_symbol, found_type) => (
                          case found_type of 
                              Types.BOTTOM =>
                                (ErrorMsg.error pos "field not found in record \n";
                                {exp=(), ty=Types.BOTTOM})
                        
                          | (_) => {exp=(), ty = found_type}
                        )
                      | NONE => (ErrorMsg.error pos "field not found in record \n";
                                {exp=(), ty=Types.BOTTOM})
                    )
                    end
                  
                | (_) => (
                  ErrorMsg.error pos "get field on nonrecord type \n";
                  {exp=(), ty=Types.BOTTOM}
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
            if tyEqualTo(#ty (transExp(venv, tenv, test)), Types.INT)
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
                  if not (tyEqualTo(#ty (transExp(venv, tenv, valOf else') ), #ty thenTyp)) then ( 
                  ErrorMsg.error pos "branches have non-matching types" ;
                  {exp=(), ty=Types.NIL}
                  ) else (
                    thenTyp
                  )
                )
                else (
                  (* check that then' is of UNIT type*)
                if not (tyEqualTo((#ty thenTyp), Types.UNIT)) then ( ErrorMsg.error pos "if-then statements must return unit."; {exp=(), ty=Types.NIL})
                else {exp=(), ty=Types.UNIT}
                )
              end
            )
          | Absyn.WhileExp({test: Absyn.exp, body: Absyn.exp, pos: Absyn.pos}) => 
            (* check test is of type int*)
            (let 
              val testType = #ty (transExp(venv, tenv, test))
              val lol = print("in whileExp\n")
            in
              (
              if tyEqualTo(testType, Types.INT)
              then
                (
                  let 
                    val bodyType = #ty (transExp(venv, tenv, body))   
                  in
                    (if not (tyEqualTo(bodyType, Types.UNIT))
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

           | Absyn.ForExp({var: Symbol.symbol, escape: bool ref, lo: Absyn.exp, hi: Absyn.exp, body: Absyn.exp, pos: Absyn.pos}) =>
            (* Remember to call enterScope here !!*)
            (* First, check exp1 is Types.INT*)
            ( let val loType = #ty (transExp(venv, tenv, lo))
              in
                (if not (tyEqualTo(loType, Types.INT))
                 then (
                  ErrorMsg.error pos "ForExp lower bound is not int type";
                  {exp=(), ty=Types.NIL}
                  )
                 else (
                  (* Check type of hi *)
                    let 
                       val hiType = #ty (transExp(venv, tenv, hi))
                    in  
                      (if not (tyEqualTo(hiType, Types.INT))
                       then (
                        ErrorMsg.error pos "ForExp upper bound is not int type";
                        {exp=(), ty=Types.NIL}
                       )
                       else (
                          (* Save var in venv *)
                          (* Rmb to enter scope *)
                          let 
                              val venv = Symbol.enter(venv, var, Env.VarEntry({ty=Types.INT}))
                          in
                             (let val bodyType = #ty (transExp(venv, tenv, body))
                              in
                              (  
                                if not (tyEqualTo(bodyType, Types.UNIT))
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
            ) 
          
          | Absyn.CallExp({func:Symbol.symbol, args: Absyn.exp list, pos: Absyn.pos}) =>
            (* Compare type of each exp in exp list against param list of func*)
            let 
                val expectedParamsResultTyOpt = Symbol.look(venv, func)
                val actualParamsTyList = map (fn(currentExp) => (#ty (transExp(venv, tenv, currentExp)))) args
            in
              if isSome expectedParamsResultTyOpt
              then (
                let val expectedParamsResultTy = valOf(expectedParamsResultTyOpt)
                in (
                  case expectedParamsResultTy of Env.FunEntry({formals = expectedParamsTyList, result = resultTy}) =>
                    
                    if tyListEqualTo(expectedParamsTyList, actualParamsTyList)
                    then ({exp=(), ty=resultTy})
                    else (
                      ErrorMsg.error pos "Illegal argument types passed to CallExp\n";
                      {exp=(), ty=Types.NIL}
                    )
                  | (_) => (ErrorMsg.error pos "Non-function used in CallExp\n";{exp=(),ty=Types.NIL})
                )
                end
              ) else (
                (* function undeclared *)
                ErrorMsg.error pos "undeclared function";
                {exp=(), ty=Types.NIL}
              )
          end
          
           | Absyn.AssignExp({var:Absyn.var, exp:Absyn.exp, pos:Absyn.pos}) =>
              (* Call transvar on var, get its type, compare with exp*)
              let val varTyp = (#ty (transVar(venv, tenv, var)))
                  val expTyp = (#ty (transExp(venv, tenv, exp)))
              in
                if not (tyEqualTo(varTyp, Types.NIL)) then
                (
                  if not (tyEqualTo(varTyp, expTyp)) then
                  (
                    ErrorMsg.error pos "AssignExp lvalue type and exp type don't match"
                  )
                  else ();
                  {exp=(), ty=Types.UNIT}
                )
                else (
                  ErrorMsg.error pos "variable type was found to be NIL.";
                  {exp=(), ty=Types.UNIT}
                )
              end
          | Absyn.RecordExp({fields, typ: Symbol.symbol, pos: Absyn.pos}) =>

              (* look up typ in tenv, get field list, tycheck each field *)
              let 
                val found_tup_opt = Symbol.look(tenv, typ)
  
                val sorted_fields = ListMergeSort.sort (fn(x,y) => Symbol.name(#1 x) > Symbol.name(#1 y)) fields
                val sorted_exp_fields = map (fn x => (#1 x, #ty(transExp(venv, tenv, #2 x)))) sorted_fields

              in(
                  case found_tup_opt of 
                  SOME(Types.RECORD(get_fields_func, unique_ref)) =>
                    (
                    let val found_list = get_fields_func()
                        val sorted_found_list = ListMergeSort.sort (fn(x,y) => Symbol.name(#1 x) > Symbol.name(#1 y)) found_list
                        val zipped_list = ListPair.zip (sorted_found_list, sorted_exp_fields) (*  ( (sym1,ty1),(sym2,ty2) )  *)
                    in (
                        if List.length(sorted_found_list) = List.length(sorted_exp_fields)
                        then (
                          foldl (fn (next_tup, equalSoFar) => if  tyEqualTo((#2 (#1 next_tup)),(#2 (#2 next_tup))) then equalSoFar else false) true zipped_list;
                          {exp=(), ty = Types.RECORD(get_fields_func, unique_ref)}
                        ) else (
                          ErrorMsg.error pos "Record creation does not assign value to all fields";
                          {exp=(), ty = Types.BOTTOM}
                        )
                      )
                    end
                    )
                  | NONE => (
                    ErrorMsg.error pos "Unknown record type instantiated";
                    {exp=(), ty = Types.BOTTOM}
                  )
                 )
              end
          
          |_ => (
            ErrorMsg.error 0 "unmatched exp";
            {exp=(), ty=Types.NIL}
          )
        )
  
      (* Just handle non-recursive tydecs first *)
      and transDec(venv, tenv, dec): {venv:Env.enventry Symbol.table, tenv:Types.ty Symbol.table} =
        case dec of Absyn.TypeDec(tydecList) =>
          (* add non array/record to tenv; create table symbol=>unit ref for array/record; *)
          let 
              val unitRefTable = createUnitRef tydecList
              val (list,err) = foldl checkDupsFoldFunc ([],0) tydecList
              fun foldFn2({name=name_symbol, ty=ty, pos=pos}, table) = 
                case ty of 
                   Absyn.NameTy(ty_symbol,pos) => Symbol.enter(table, name_symbol, [ty_symbol])
                  | Absyn.RecordTy(fieldlist) => 
                    let 
                      fun fieldTypeAccumulator(field, res) = case field of 
                      {name=name_symbol, escape=_, typ=ty_symbol, pos=pos} => ty_symbol::res
                      val fieldtypelist = foldr fieldTypeAccumulator [] fieldlist
                    in
                      Symbol.enter(table, name_symbol, fieldtypelist)
                    end
                  | Absyn.ArrayTy(ty_symbol, pos) => Symbol.enter(table, name_symbol, [ty_symbol])
              val symTable = foldl foldFn2 Symbol.empty tydecList
          in (
            if err = 1 
            then 
            ( 
              {venv=venv, tenv=tenv}
            )
            else 
             ( 
               let fun foldFn (somety,tenv) =
                case somety of {name=name, ty=ty, pos=pos} =>
                  ( print("folding transty over one typedec\n");
                  Symbol.enter(tenv, name, transTy(name, tenv,ty,tydecList, symTable, unitRefTable, []))
                  )
               in 
                  {venv=venv, tenv=foldl foldFn tenv tydecList}
               end
            )
          )
        end
          
        | Absyn.VarDec({name, escape, typ, init, pos})	=> (* how to handle escape? what is it even??? *)
  	      (* if isSome typ then need to do type checking of init against typ, else just save the type of init for var name in venv*)
  	      if isSome typ then
            (
            let val expectedType = getTypeOrNil(tenv, #1 (valOf typ))
  	        in
  	          case expectedType of Types.NIL =>
                (
  	              ErrorMsg.error pos "Used Nil as a type.";
  	              {venv=venv, tenv=tenv}
  	            )  	          
              | (_) => 
                (
    	            let val actualType = (#ty (transExp(venv, tenv, init)))
        	          val newVarEntry = Env.VarEntry({ty=actualType})
        	        in (
      	              (* begin of case *)
      	            case (actualType, expectedType) of (Types.ARRAY(sym, reff),Types.ARRAY(sym2, reff2)) => (
      	                  if tyEqualTo(sym(), sym2()) then (
            	              let 
            	                  val venv = Symbol.enter(venv, name, newVarEntry) 
            	              in 
              	                {venv=venv, tenv=tenv}
              	            end
          	              ) else (
        	                  ErrorMsg.error pos "Declared type does not match exp type";
        	                  {venv=venv, tenv=tenv}
          	              ) 
      	                 ) 
      	            | (x, y) => (
      	                  if tyEqualTo(actualType, expectedType)
      	                  then 
        	                    let 
          	                      val venv = Symbol.enter(venv, name, newVarEntry) 
          	                  in 
          	                      {venv=venv, tenv=tenv}
            	                end
            	            else  (
      	                      ErrorMsg.error pos "Declared type does not match exp type.";
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
                    if tyEqualTo(expectedReturnType, actualRetType)
                    then true
                    else (ErrorMsg.error pos "function return type mismatch\n";false)
                  end
              )
          in
            if (foldl foldFn true fundeclist) then {venv=venv, tenv=tenv} else (ErrorMsg.error 0 "fundecs do not type check\n"; {venv=venv, tenv=tenv})
          end))
            
            (*| (_) => ( 
              ErrorMsg.error 0 "unmatched dec";
              {venv=venv, tenv=tenv}
            )*)
          
  (*
  fun recursiveSearch (name: Symbol.symbol, tenv, symTable, currentPath: Symbol.symbol list, recordSeen: bool): Types.ty =
    (* 
    symTable: symbol -> symbol list table 
    tenv: local tenv created by get_fields
    
    in search function :
    
    if sym matches something in local_tenv, return that if it is base type or recurse over it if it isnt.
    2.1 raise error if illegal cycle detected i.e. cycles with no array or record type
    2.2 add newly resolved types to local_tenv  *)
    
    let val newPath = name :: currentPath
    in
      let
  *)
    
  and transTy(name, tenv, Absyn.RecordTy(fieldlist:{escape:bool ref, name:Symbol.symbol, pos:Absyn.pos,
                    typ:Symbol.symbol} list), decList, symTable ,unitRefList, currentPath: Symbol.symbol list): Types.ty = 
    let
      fun get_fields () = (print("calling getfields");
                           map (fn {name,escape,typ,pos} => 
                          let val nextTypOpt = getTyOfName(decList, typ)
                              val foundInTenvOpt = Symbol.look(tenv, typ)
                          in
                            if isSome nextTypOpt then
  		                        (name, transTy(name, tenv, #ty (valOf nextTypOpt), decList, symTable, unitRefList, []))
  		                      else 
  		                        if isSome foundInTenvOpt then (name, (valOf foundInTenvOpt)) else (
  		                        print(Symbol.name(typ));
  		                        ErrorMsg.error pos "[GET FIELDS] Symbol is not found in local tenv or decList";
  		                        (name, Types.BOTTOM))
  		                    end
		              ) fieldlist)
    in
    
      (print("in transty for recordty\n");
      printGetFieldsOutput(get_fields());
      let val unitRef =
        let val unitRefOpt = Symbol.look(unitRefList, name) in
          case unitRefOpt of SOME(x) => x
          | NONE => (ErrorMsg.error 0 "Unit ref not found for record type.";
                   ref ()
                  )
        end
      in
        Types.RECORD (get_fields,unitRef)
      end
      )
    end
      
    
  
    | transTy(name, tenv, Absyn.NameTy(ty_sym, pos), decList, symTable ,unitRefList, currentPath: Symbol.symbol list): Types.ty = 
    let 
      val updatedPath = name :: currentPath
      val updatedPathString = map (fn x => print(Symbol.name(x))) updatedPath
    in 
     
    (
      print("\n");
      (* get fields pointed to by current record from sym table*)
      let 
        val foundOpt = List.find (fn x => x = ty_sym) updatedPath 
          in 
            case foundOpt of SOME(x) => 
              (* Cycle with no record type detected, we know this because we do not add record types to list *)
              (* Error, illegal cycle*)
              (
              ErrorMsg.error pos "Illegal cycle in type dec!";
              Types.BOTTOM
              )
            | NONE =>
              let 
                val tySymOpt = getTyOfName (decList, ty_sym)
                val foundInTenvOpt = Symbol.look(tenv, ty_sym)
              in
                if isSome tySymOpt then (
                  transTy(ty_sym, tenv, #ty (valOf(tySymOpt)), decList, symTable ,unitRefList, updatedPath)
                  )
                else
                  (* search tenv*)
                  if isSome foundInTenvOpt then
                    valOf foundInTenvOpt
                  else (
                    ErrorMsg.error pos "Type declarations do not resolve.";
                    Types.BOTTOM
                  )
              end
      end)
    end
    | transTy(name, tenv, Absyn.ArrayTy(ty_sym, pos), decList, symTable ,unitRefList, currentPath: Symbol.symbol list): Types.ty = 
      Types.ARRAY((fn() => Types.INT), ref ()) (* TEMP *)


  and transVar(venv:venv, tenv:tenv, var:Absyn.var) =
    (*pattern match on Absyn.var*)
    case var of Absyn.SimpleVar(sym, pos) =>
      let val varTypOpt = Symbol.look(venv, sym)
      in 
        (if isSome varTypOpt 
        then (
        let val varTyp = valOf varTypOpt
          in 
            case varTyp of
              Env.VarEntry({ty=typ}) => {exp=(), ty=typ}
            | _ => (
                    ErrorMsg.error pos "using function as a var";
                    {exp=(), ty=Types.NIL} 
                   )
          end
        )
        else
          (
           ErrorMsg.error pos "var was not declared";
           {exp=(), ty=Types.NIL}
          )
        )
      end

    
      | Absyn.FieldVar(var, symbol, pos) =>
      (* *)
      let 
        val varType = #ty (transVar(venv,tenv,var))
      in
        if not (tyEqualTo(varType, Types.RECORD((fn()=>[]), ref())))
        then (
          ErrorMsg.error pos "accessing field of non-fieldVar\n";
          {exp=(), ty=Types.NIL}
        ) 
        else (
          case varType of
            Types.RECORD(fnn, reff) => 
              let 
                val fieldList = fnn()
                fun matchFn (tup:Symbol.symbol * Types.ty) = ((#1 tup) = symbol)
                val fieldVal = List.find matchFn fieldList
             in 
              (
                (* check if symbol exists in field list *)
                (*val find : ('a -> bool) -> 'a list -> 'a option*)
                case fieldVal of 
                  NONE => (
                    ErrorMsg.error pos "nonexistent field\n";
                    {exp=(), ty=Types.NIL}
                  )
                  | SOME (sym,typ) => {exp=(),ty=typ}
              )
              end
        )
      end
      
  
        
    | Absyn.SubscriptVar(var, exp, pos) =>
      (* check exp is int *)
      (* check var is array *)
      let 
          val expType = #ty (transExp(venv:venv,tenv:tenv,exp))
          val varType = #ty (transVar(venv:venv,tenv:tenv,var))
      in (
         if not (tyEqualTo(expType,Types.INT))
         then (
            ErrorMsg.error pos "Non-integer used as array index.\n";
            {exp=(), ty=Types.NIL}
         )
         else (
            case varType of 
                Types.ARRAY(arrayGetterFn, someref) => {exp=(),ty=arrayGetterFn()}
              | _ => (
              ErrorMsg.error pos "subscript var on nonarray type.\n";
              {exp=(), ty=Types.NIL}
            )
          )
      )
      end
      
  in
    transExp(venv,tenv, e);
    print "jobs done!"
  end
  


end
