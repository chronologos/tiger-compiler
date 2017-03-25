structure Env :> ENV =
struct
  type access = int
  (*type ty = Types.ty*)
  datatype enventry = VarEntry of {access: Translate.access, ty: Types.ty}
                    | FunEntry of {level: Translate.level,
                                   label: Temp.label,
                                   formals: Types.ty list,
                                   result: Types.ty}

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
      val printFunEntry = FunEntry({level=Translate.outermost,label=Temp.newlabel(),formals=[Types.STRING], result = Types.UNIT})
      val flushFunEntry = FunEntry({level=Translate.outermost,label=Temp.newlabel(),formals=[], result=Types.UNIT})
      val getcharFunEntry = FunEntry({level=Translate.outermost,label=Temp.newlabel(),formals = [], result = Types.STRING})
      val ordFunEntry = FunEntry({level=Translate.outermost,label=Temp.newlabel(),formals = [Types.STRING], result = Types.INT})
      val charFunEntry = FunEntry({level=Translate.outermost,label=Temp.newlabel(),formals = [Types.INT], result=Types.STRING})
      val sizeFunEntry = FunEntry({level=Translate.outermost,label=Temp.newlabel(),formals=[Types.STRING], result=Types.INT})
      val substringFunEntry = FunEntry({level=Translate.outermost,label=Temp.newlabel(),formals=[Types.STRING, Types.INT, Types.INT], result = Types.STRING})
      val concatFunEntry = FunEntry({level=Translate.outermost,label=Temp.newlabel(),formals=[Types.STRING, Types.STRING], result=Types.STRING})
      val notFunEntry = FunEntry({level=Translate.outermost,label=Temp.newlabel(),formals = [Types.INT], result = Types.INT})
      val exitFunEntry = FunEntry({level=Translate.outermost,label=Temp.newlabel(),formals=[Types.INT], result=Types.UNIT})
      val nilEntry = VarEntry({access=Translate.allocLocal(Translate.outermost)(true) ,ty=Types.NIL})
      val t1 = Symbol.enter(emptyTable, Symbol.symbol("print"), printFunEntry)
      val t2 = Symbol.enter(t1, Symbol.symbol("flush"), flushFunEntry)
      val t3 = Symbol.enter(t2, Symbol.symbol("getchar"), getcharFunEntry)
      val t4 = Symbol.enter(t3, Symbol.symbol("ord"), ordFunEntry)
      val t5 = Symbol.enter(t4, Symbol.symbol("chr"), charFunEntry)
      val t6 = Symbol.enter(t5, Symbol.symbol("size"), sizeFunEntry)
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
  (*transVar: venv * tenv * Absyn.var * level -> expty*)
  (*transExp: venv * tenv * Absyn.exp * level -> expty*)
  (*transDec: venv * tenv * Absyn.dec * level -> {venv: venv, tenv: tenv}*)
  (*transTy:  tenv * Absyn.ty -> Types.ty*)
  (*transProg: Absyn.exp -> unit*)
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table
  type expty = {exp: unit, ty: Types.ty}

  val venv = Env.base_venv
  val tenv = Env.base_tenv
  val breakable = ref 0;
  val debug = false;

  fun debugPrint(msg:string, pos:int) =
    if debug
    then ErrorMsg.error pos msg
    else ()

  fun getTypeOrBottom(table, sym) =
    let
      val resOpt = Symbol.look(table, sym)
    in
      if isSome resOpt then valOf resOpt else Types.BOTTOM
    end

(*  fun inEnv(env, sym) =
    if isSome (Symbol.look(env, sym)) then print("inside\n") else print("not inside\n")
*)

  fun printDatatype (Types.RECORD(_)) = print "RECORD"
  | printDatatype Types.INT = print "INT"
  | printDatatype Types.STRING = print "STRING"
  | printDatatype Types.UNIT = print "UNIT"
  | printDatatype Types.BOTTOM = print "BOTTOM"
  | printDatatype (Types.ARRAY(_)) = print "ARRAY"
  | printDatatype Types.NIL = print "NIL"

  (* add record type symbols to someEnv and create unit ref *)
  fun unitRefFolder ({name=name, ty=Absyn.RecordTy(fieldList), pos=pos}, someEnv) =
    (
      (*print(Symbol.name(name));
      print("\n"); *)
      Symbol.enter(someEnv, name, ref ())
    )
  | unitRefFolder({name=name, ty=Absyn.ArrayTy(sym, p), pos=pos}, someEnv) =
    ( (*print(Symbol.name(name));
      print("\n");
      *)
      Symbol.enter(someEnv, name, ref ())
    )
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
          ErrorMsg.error (#pos (currentDec)) "Duplicate declaration in type declist.";
          (listOfNames, 1)
        )
        else(
          ((#name (currentDec)) :: listOfNames, 0)
        )
      )
    end

	fun checkFunDupsFoldFunc(currentDec as {name=name, params=params, result=result, body=body, pos=pos}, (listOfNames, err)) =
	   let
        val found = List.find (fn(x) => x = (#name (currentDec))) listOfNames
      in
        if err = 1
        then (listOfNames,err)
        else (
          if isSome found
          then (
            ErrorMsg.error (#pos currentDec) "Duplicate declaration in fun declist.";
            (listOfNames, 1)
          )
          else(
            ((#name (currentDec)) :: listOfNames, err)
          )
        )
      end

  fun paramsToFormals(params:{name:Symbol.symbol,escape: bool ref, typ: Symbol.symbol, pos:Absyn.pos} list) =
    let
      fun foldFn (p,list) = 
        case p of
          {name=name,escape=ecp, typ=typ, pos=pos} => (debugPrint("escape of formal "^Symbol.name(name)^" is "^Bool.toString(!ecp)^".\n",pos); !ecp::list)
    in
      foldr foldFn [] params
    end

  fun tyEqualTo(ty1, ty2) =
    case (ty1, ty2) of
      (Types.RECORD(_), Types.RECORD(_)) => true
    | (Types.ARRAY(_), Types.ARRAY(_)) => true
    | (Types.INT, Types.INT) => true
    | (Types.STRING, Types.STRING) => true
    | (Types.NIL, Types.NIL) => false
    | (Types.UNIT, Types.UNIT) => true
    | (Types.BOTTOM, _) => true
    | (_, Types.BOTTOM) => true
    | (_, _) => false

  fun recordTyEqualTo(rec1Ty, rec2Ty) =
    case (rec1Ty, rec2Ty) of (Types.RECORD(get_fields_1, unique_ref_1), Types.RECORD(get_fields_2, unique_ref_2)) =>
      unique_ref_1 = unique_ref_2
    | (_) => (print("Non-record types passed into recordTyEqualTo method!"); false)

  fun arrayTyEqualTo(Types.ARRAY(f1, r1), Types.ARRAY(f2, r2)) = (r1 = r2)
    | arrayTyEqualTo(_, _) = (print("Non-array types passed into arrayTyEqualTo method!"); false)

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
  fun addFunctionParamsVenv(venv,tenv,params:{name:Symbol.symbol,escape: bool ref, typ: Symbol.symbol, pos:Absyn.pos} list, funLevel) =
    let
      val frameAccessList = Translate.formals funLevel
      val zipped = ListPair.zip (params,frameAccessList)
      fun foldFn (x, venv) =
        case x of
        ({name:Symbol.symbol,escape: bool ref, typ: Symbol.symbol, pos:Absyn.pos},acc) =>
          let
            val paramType = getTypeOrBottom(tenv,typ)
          in
            case paramType of
              Types.BOTTOM  => (ErrorMsg.error pos "Unknown parameter type for function"; venv)
            | _ => Symbol.enter(venv,name,Env.VarEntry({access=acc,ty=paramType}))
          end
    in
      foldl foldFn venv zipped
    end

  (* Params: name = function name; params = (symbol*type) list; resultVal = symbol*pos
     Returns: function entry to venv *)
  fun parseHeaders (name, params:{name: Symbol.symbol, escape: bool ref,
      typ: Symbol.symbol, pos: Absyn.pos} list, tenv) =
      (let
          val params_sym = map (fn x => ( #typ x)) params
        val paramsTyList =
          (* transTy on #1 resultVal *)
          let fun funcFoldrArg(sym, result) =
            let
              val foundType = getTypeOrBottom(tenv, sym)
            in (
              case foundType of Types.BOTTOM =>
                (ErrorMsg.error 0 "Unknown type in params list"; Types.BOTTOM :: result)
              | (_) => foundType :: result
             )
            end
          in
            foldr funcFoldrArg [] params_sym
          end
      in
        paramsTyList
      end)

  fun venvWithFunctionHeaders (venv, fundeclist, tenv, level) =
  let
    fun parseHeadersWithReturnType(x, venv) =
        case x of {name=name, params=params , result=result, body=body, pos=pos} =>
          (case result of SOME(symbol,pos) => (
            (* first pass put function headers into venv table *)
            let
              val paramsList = parseHeaders (name, params, tenv)
              val resultTy =
              let val typeGotten = getTypeOrBottom(tenv, symbol)
              in
                case typeGotten of Types.BOTTOM =>  (ErrorMsg.error pos "return type not found in tenv"; Types.BOTTOM)
                | (_) => typeGotten
              end
              val funentry = Env.FunEntry({level=level,label=Temp.newlabel(),formals=paramsList, result=resultTy})
            in
              (*
              (* print("storing function with non unit type as result: "); *)
              (* printDatatype(resultTy); *)
              (*print("\n"); *)
              *)
              Symbol.enter(venv, name, funentry)
            end
            (* this helper is called if isSome result, else return type is known as UNIT and is put in table*)
          )
          | NONE => (
            let
              val paramsList = parseHeaders (name, params, tenv)
              val funentry = Env.FunEntry({level=level,label=Temp.newlabel(),formals=paramsList,result=Types.UNIT})
            in
              (*print("storing function with type unit as result"); *)
              Symbol.enter(venv, name, funentry)
            end
          ))
    in
            foldl parseHeadersWithReturnType venv fundeclist
    end

  fun getTyOfName(declist : {name: Symbol.symbol, ty: Absyn.ty, pos: Absyn.pos} list, name) =
    let val foundOpt = List.find (fn(x)=>(#name x) = name) declist
    in
      foundOpt
    end
  (*
  fun getSymFromFieldList(sym, fieldlist) =
    let fun getField(currentField:Symbol.symbol * Types.ty, typSoFar:Types.ty) =
      if Symbol.name(#1 (currentField) ) = Symbol.name(sym)
      then #2 (currentField)
      else typSoFar
    in
        foldl getField Types.BOTTOM fieldlist
    end
  *)

  fun transProg(e) =
    let
      fun transOpExp(venv, tenv, Absyn.OpExp({left,oper,right,pos}), level, looplabel) =
      let
        val {exp=expLeft, ty=tyLeft} = transExp(venv,tenv,left,level,looplabel)
        val {exp=expRight, ty=tyRight} = transExp(venv,tenv,right,level,looplabel)
      in ( 
        case oper of
          (Absyn.PlusOp |  Absyn.MinusOp | Absyn.TimesOp | Absyn.DivideOp) =>
            (
              case (tyLeft, tyRight) of (Types.INT, Types.INT) => {exp=Translate.opExp(expLeft,expRight,oper),ty=Types.INT}
              | (_, _) => (ErrorMsg.error pos "Integer required in OpExp."; {exp=Translate.transError(), ty=Types.BOTTOM})
            )
          | (Absyn.GeOp | Absyn.GtOp | Absyn.LeOp | Absyn.LtOp) => (
              case (tyLeft, tyRight) of (Types.INT, Types.INT) =>
                {exp=Translate.opExp(expLeft,expRight,oper),ty=Types.INT}
              | (Types.STRING, Types.STRING) => {exp=Translate.strcmp(expLeft,expRight,oper,level),ty=Types.INT}
              | (_,_) => (ErrorMsg.error pos "Both operands must be either int or string"; {exp=Translate.transError(), ty=Types.BOTTOM})
            )
          | (Absyn.NeqOp | Absyn.EqOp) =>

            case (tyLeft, tyRight) of (Types.STRING, Types.STRING) => {exp=Translate.strcmp(expLeft,expRight,oper,level),ty=Types.INT}
            | (Types.INT, Types.INT) => {exp=Translate.opExp(expLeft,expRight,oper), ty=Types.INT}
            | (Types.RECORD(_ , x),Types.RECORD(_, y))  => 
              if x = y then
                {exp=Translate.refCompare(expLeft,expRight,oper), ty=Types.INT} 
              else (
                ErrorMsg.error pos "Both operands must be of the same type.";
                {exp=Translate.transError(), ty=Types.BOTTOM}
              )
            | (Types.ARRAY(_ , x), Types.ARRAY(_,y))=> if x = y then {
              exp=Translate.refCompare(expLeft,expRight,oper), ty=Types.INT} else (
              ErrorMsg.error pos "Both operands must be of the same type";
              {exp=Translate.transError(), ty=Types.BOTTOM}
             )
            | ((Types.NIL, Types.RECORD(_, x)) | (Types.RECORD(_, x), Types.NIL))
             => {exp=Translate.refCompare(expLeft,expRight,oper), ty=Types.INT}
            | (_, _) =>  (
              ErrorMsg.error pos "Both operands must be of the same type";
              {exp=Translate.transError(), ty=Types.BOTTOM}
            )
      )
      end

      and transExp(venv:venv, tenv:tenv, exp, level, looplabel): {exp:Translate.exp, ty:Types.ty} = (
        case exp of Absyn.IntExp(_) => (
          if debug
          then print("IntExp at level "^Translate.levelToString(level)^".\n")
          else ();
          {exp=Translate.intExp(exp), ty=Types.INT}
        )
          | Absyn.NilExp => (if debug
          then print("NilExp at level "^Translate.levelToString(level)^".\n")
          else ();{exp=Translate.nilOrUnit(), ty=Types.NIL})
          | Absyn.BreakExp(pos) => (if debug
          then print("BreakExp at level "^Translate.levelToString(level)^".\n")
          else (); if !breakable < 1 then (ErrorMsg.error pos "Illegal break."; {exp=Translate.transError(),ty=Types.BOTTOM}) else {exp=Translate.transBreak(looplabel),ty=Types.BOTTOM})
          | Absyn.StringExp(_) => (if debug
          then print("StringExp at level "^Translate.levelToString(level)^".\n")
          else ();{exp=Translate.stringExp(exp),ty=Types.STRING})
          | Absyn.OpExp(_) => (if debug
          then print("OpExp at level "^Translate.levelToString(level)^".\n")
          else ();transOpExp(venv, tenv, exp, level,looplabel))
          | Absyn.ArrayExp({typ=t, size=s, init=i, pos=pos}) => (
            (* check size is int *)
            let val {exp=sizeExp, ty=sizeTyp} = transExp(venv,tenv,s,level,looplabel)
                val dummy = debugPrint("ArrayExp at level "^Translate.levelToString(level)^".\n",pos)
            in
              case sizeTyp of
                Types.INT => (
                  (* check init same type as t *)
                  let
                    val {exp=initExp,ty=initTyp} = transExp(venv,tenv,i,level,looplabel)
                    val tTyp = case Symbol.look(tenv, t) of 
                      SOME(x) => x
                    | NONE =>  (
                                ErrorMsg.error pos "Undeclared type.";
                                Types.BOTTOM
                               )
                  in
                    case tTyp of Types.ARRAY(array_func, _) =>
                      if tyEqualTo(initTyp, array_func()) then (
                        {exp=Translate.arrayExp(sizeExp,initExp,level), ty=tTyp}
                      ) else (
                        ErrorMsg.error pos "Type mismatch in arrayexp.";
                        {exp=Translate.transError(), ty=Types.BOTTOM}
                      )
                    | (_) => (ErrorMsg.error pos "Tried to initialize array with non array type."; {exp=Translate.transError(), ty=Types.BOTTOM})
                  end
                  )
                |(_) => (
                  ErrorMsg.error 0 "Array size must be an int.";
                  {exp=Translate.transError(), ty=Types.BOTTOM}
                )
          end
          )
          | Absyn.VarExp(somevar) => transVar(venv, tenv, somevar, level, looplabel)
          
        (* 
            let
              val res = transVar(venv, tenv, somevar, level, looplavel)
              val resTy = #ty res
              val resExp = #exp res
            in 
              case resTy of
                    SOME(Env.FunEntry(_)) => (
                    ErrorMsg.error pos "Fun not found in venv.";
                    {exp=Translate.transError(),ty=Types.BOTTOM}
                    )
                  | SOME(Env.VarEntry({access=acc,ty=ty})) => (
                    {exp=Translate.simpleVar(acc,level), ty=ty})
                  | NONE => (
                    ErrorMsg.error pos "Var not found in venv.";
                    {exp=Translate.transError(),ty=Types.BOTTOM}
                  )               
 
            ( case somevar of Absyn.SimpleVar(sym,pos) =>
              let
                  val dummy = debugPrint("varExp at level "^Translate.levelToString(level)^".\n",pos)
                  val somety = Symbol.look(venv, sym)
              in
                  case somety of
                    SOME(Env.FunEntry(_)) => (
                    ErrorMsg.error pos "Fun not found in venv.";
                    {exp=Translate.transError(),ty=Types.BOTTOM}
                    )
                  | SOME(Env.VarEntry({access=acc,ty=ty})) => (
                    {exp=Translate.simpleVar(acc,level), ty=ty})
                  | NONE => (
                    ErrorMsg.error pos "Var not found in venv.";
                    {exp=Translate.transError(),ty=Types.BOTTOM}
                  )

              end


            | Absyn.SubscriptVar(var, exp, pos) => (
              let
                val varType = (#ty (transVar(venv, tenv, var, level,looplabel)));
              in  (
                (* check type of subscript.var *)
                (* check if its an array type*)
                (*printDatatype(varType);
                print("\n"); *)
                case varType of
                  (Types.ARRAY(typ, unique)) =>
                  (* check if exp is int type *)
                  let
                    val {exp=subscriptExp, ty=expType} =  (transExp(venv, tenv, exp, level,looplabel))
                  in
                    if tyEqualTo(expType, Types.INT) then {exp=,ty=typ} else
                      (ErrorMsg.error pos "Array index must be integer.";
                      {exp=Translate.transError(), ty = Types.BOTTOM})
                  end

                | (_) => (ErrorMsg.error pos "SubscriptVar on non array type.";
                         {exp=Translate.transError(), ty=Types.BOTTOM}
                         )
                )
              end
              )
           | Absyn.FieldVar(var,symbol,pos) =>
              (* var must be record type, get field list, check if symbol is in fieldlist, return type of symbol *)
              let val varTyp = #ty (transVar(venv, tenv, var, level))
              in
                case varTyp of
                  Types.RECORD(get_fields_func, unit_ref) =>
                    let
                      val fields_list = get_fields_func()
                      val typ_opt = List.find (fn x => Symbol.name(#1 (x)) = Symbol.name(symbol)) fields_list
                    in (
                      case typ_opt of
                        SOME(found_symbol, found_type) => (
                          case found_type of
                              Types.BOTTOM =>
                                (ErrorMsg.error pos "Field not found in record.";
                                {exp=Translate.transError(), ty=Types.BOTTOM})

                          | (_) => {exp=(), ty = found_type}
                        )
                      | NONE => (ErrorMsg.error pos "Field not found in record.";
                                {exp=Translate.transError(), ty=Types.BOTTOM})
                    )
                    end

                | (_) => (
                  ErrorMsg.error pos "Tried to get field on non record type.";
                  {exp=Translate.transError(), ty=Types.BOTTOM}
                )
              end
          *)
          
          | Absyn.LetExp({decs, body, pos}) =>
            let
              fun first  (a, _) = a
              fun second (_, b) = b
              val dummy = debugPrint("LetExp at level "^Translate.levelToString(level)^".\n",pos)
              val myLevel = level
              fun foldFn (dec,({venv=venv,tenv=tenv},expList)) = 
                let val {venv=venv,tenv=tenv,exp=expOpt} = transDec(venv, tenv, dec, myLevel, looplabel)
                    val newEnv = {venv=venv,tenv=tenv}
                in
                    if isSome expOpt then
                      (newEnv, valOf(expOpt) :: expList)
                    else
                      (newEnv, expList)
                end
              val (envs,expList) = foldl foldFn ({venv=venv, tenv=tenv},[]) decs
              val expList = (List.rev expList)
              (* we want to process from left to right but preserve order of expList *)
              val {exp=bodyExp,ty=bodyTyp} = transExp(#venv envs,#tenv envs, body, myLevel,looplabel)
            in
              {exp=Translate.letExp(expList, bodyExp), ty=bodyTyp}
            end
          | Absyn.SeqExp(xs) => let
            val dummy = debugPrint("SeqExp at level "^Translate.levelToString(level)^".\n",0)
            (* need Translate.exp list to pass to Translate.seqExp *)
            fun foldSeqExp((absExp,pos),(treeExpList,endType)) = 
                let val {exp=treeExp,ty=typ} = transExp(venv, tenv, absExp, level, looplabel)
                in
                    (treeExp::treeExpList,typ)
                end
            val (treeExpList,typ) = foldr foldSeqExp ([], Types.BOTTOM) xs
            in
              {exp=Translate.seqExp(treeExpList),ty=typ}
            end
          | Absyn.IfExp({test: Absyn.exp, then': Absyn.exp, else': Absyn.exp option, pos: Absyn.pos}) =>
            (* get the type of test, must be int *)
            let 
                val {exp=testExp,ty=testTyp} = transExp(venv, tenv, test, level,looplabel)
            in
                if (not (tyEqualTo(testTyp, Types.INT)))
                then
                (
                ErrorMsg.error pos "test of if-else does not evaluate to int";
                {exp=Translate.transError(), ty=Types.BOTTOM}
                )
                else (
                  let
                    val thenTyp = transExp(venv, tenv, then', level, looplabel)
                    val {exp=thenExp,ty=typ} = thenTyp
                  in
                    if isSome else'
                    then (
                      let
                          val {exp=elseExp,ty=elseType} = transExp(venv, tenv, valOf else', level, looplabel)
                          val thenType = #ty thenTyp
                      in
                          case (thenType, elseType) of ((Types.RECORD(_), Types.NIL) | (Types.NIL, Types.RECORD(_))) =>
                            thenTyp
                          | (_, _) =>
                              if not (tyEqualTo( elseType, thenType)) then (
                                ErrorMsg.error pos "Branches have non-matching types" ;
                                {exp=Translate.transError(), ty=Types.BOTTOM}
                               ) else (
                              {exp=Translate.transIf({test=testExp, then'=thenExp, else'=SOME(elseExp), pos=pos}),ty=elseType}
                              )
                      end
                    )
                    else (
                      (* check that then' is of UNIT type*)
                    if not (tyEqualTo((#ty thenTyp), Types.UNIT)) then (
                    ErrorMsg.error pos "if-then statements must return unit.";
                    {exp=Translate.transError(), ty=Types.BOTTOM})
                    else {exp=Translate.transIf({test=testExp, then'=thenExp, else'=NONE, pos = pos}), ty=Types.UNIT}
                    )
                  end
                )
            end
          | Absyn.WhileExp({test: Absyn.exp, body: Absyn.exp, pos: Absyn.pos}) =>
            (* check test is of type int*)
            (let
              val dummy = debugPrint("WhileExp at level "^Translate.levelToString(level)^".\n",pos)
              val donelabel = Temp.newlabel()
              val test' = transExp(venv, tenv, test, level, donelabel)
              val testType = #ty test' 
            in
              (
              breakable := (!breakable + 1);
              if tyEqualTo(testType, Types.INT)
              then
                (
                  let
                    val body = transExp(venv, tenv, body, level, donelabel)
                    val bodyType = #ty body
                  in
                    (if not (tyEqualTo(bodyType, Types.UNIT))
                    then
                      (
                        breakable := (!breakable - 1);
                        ErrorMsg.error pos "While expression has non-unit body type\n";
                        {exp=Translate.transError(), ty=Types.BOTTOM}
                      )
                    else
                      (
                      breakable := (!breakable - 1);
                      {exp=Translate.transWhile(#exp test', #exp body, donelabel), ty=Types.UNIT})
                  )
                  end
                )
              else
                (
                  breakable := (!breakable - 1);
                  ErrorMsg.error pos "While expression test condition has non-int type\n";
                  {exp=Translate.transError(), ty=Types.BOTTOM}
                )
              )
            end)

           | Absyn.ForExp({var: Symbol.symbol, escape: bool ref, lo: Absyn.exp, hi: Absyn.exp, body: Absyn.exp, pos: Absyn.pos}) =>
            (* Remember to call enterScope here !!*)
            let
              val donelabel = Temp.newlabel()
              val lo' = transExp(venv, tenv, lo, level,looplabel)
              val loType = #ty lo'
              val hi' = transExp(venv, tenv, hi, level,looplabel)
              val hiType = #ty hi' 
              val forLoopLevel = level
              val dumm = debugPrint("forExp at level "^Translate.levelToString(forLoopLevel)^"\n",pos)
            in
              if not (tyEqualTo(loType, Types.INT))
               then (
                ErrorMsg.error pos "ForExp lower bound is not int type";
                {exp=Translate.transError(), ty=Types.BOTTOM}
                )
               else (
                if not (tyEqualTo(hiType, Types.INT))
                  then (
                    ErrorMsg.error pos "ForExp upper bound is not int type";
                    {exp=Translate.transError(), ty=Types.BOTTOM}
                   )
                else (
                  (* Save var in venv *)
                  breakable := (!breakable + 1);
                  let
                    (*val venv = enterscope(venv)*)
                    val loopVar = Translate.allocLocal(forLoopLevel)(!escape)
                    val venv = Symbol.enter(venv, var, Env.VarEntry({access=loopVar,ty=Types.INT}))
                    val dum = debugPrint("Calling Translate ecp="^Bool.toString(!escape)^" for var "^Symbol.name(var)^" at level "^Translate.levelToString(forLoopLevel)^".\n",pos)
                  in
                    let 
                      val body' = transExp(venv, tenv, body, forLoopLevel,looplabel) 
                      val bodyType = #ty body'
                    in
                      if not (tyEqualTo(bodyType, Types.UNIT))
                      then (
                        breakable := (!breakable - 1);
                        ErrorMsg.error pos "ForExp body not of UNIT type";
                        {exp=Translate.transError(), ty=Types.BOTTOM}
                        )
                      else (
                        breakable := (!breakable - 1);
                        {exp=Translate.transFor(loopVar, #exp lo', #exp hi', #exp body', donelabel), ty=Types.UNIT}
                        )
                    end
                  end
                )
              )
          end
            
          | Absyn.CallExp({func:Symbol.symbol, args: Absyn.exp list, pos: Absyn.pos}) =>
            (* Compare type of each exp in exp list against param list of func*)
            let
                val expectedParamsResultTyOpt = Symbol.look(venv, func)
                val actualParamsExpTyList = map (fn(currentExp) => (transExp(venv, tenv, currentExp, level, looplabel))) args
                val actualParamsTyList = map(fn {exp=treeExp,ty=typ} => typ ) actualParamsExpTyList
                val treeExpArgList = map(fn {exp=treeExp,ty=typ} => treeExp ) actualParamsExpTyList
            in
              if isSome expectedParamsResultTyOpt
              then (
                let val expectedParamsResultTy = valOf(expectedParamsResultTyOpt)
                in (
                  case expectedParamsResultTy of Env.FunEntry({level=dLevel,label=lab, formals = expectedParamsTyList, result = resultTy}) =>
                    if tyListEqualTo(expectedParamsTyList, actualParamsTyList)
                    then ({exp=Translate.callExp(dLevel,level,lab,treeExpArgList), ty=resultTy})
                    else (
                      ErrorMsg.error pos "Illegal argument types passed to CallExp.";
                      {exp=Translate.transError(), ty=Types.BOTTOM}
                    )
                  | (_) => (ErrorMsg.error pos "Non-function used in CallExp.";{exp=Translate.transError(),ty=Types.BOTTOM})
                )
                end
              ) else (
                (* function undeclared *)
                ErrorMsg.error pos "Tried to call undeclared function";
                {exp=Translate.transError(), ty=Types.BOTTOM}
              )
          end

           | Absyn.AssignExp({var:Absyn.var, exp:Absyn.exp, pos:Absyn.pos}) =>
              (* Call transvar on var, get its type, compare with exp*)
              let val {exp=varExp, ty=varTyp} = (transVar(venv, tenv, var, level, looplabel))
                  val {exp=initExp,ty=expTyp} = (transExp(venv, tenv, exp, level,looplabel))
              in

                case (varTyp, expTyp) of (Types.RECORD(_, _), Types.RECORD(_, _)) => 
                  if recordTyEqualTo(varTyp, expTyp) then {exp=Translate.assignExp(varExp,initExp), ty=Types.UNIT} 
                  else (
                    ErrorMsg.error pos "Yo Noid Record types are different";
                    {exp = Translate.transError(), ty = Types.BOTTOM}
                  )
                | (Types.ARRAY(_, _), Types.ARRAY(_, _)) => (
                  if (arrayTyEqualTo(varTyp, expTyp)) then {exp=Translate.assignExp(varExp,initExp),ty=Types.UNIT} 
                  else (
                    ErrorMsg.error pos "Array types are different.";
                    {exp=Translate.transError(), ty=Types.BOTTOM}
                  )
                  )
                | (Types.RECORD(_,_), Types.NIL) => {exp=Translate.assignExp(varExp,initExp), ty=Types.UNIT}
                | (t1, t2) => (
                  if not (tyEqualTo(varTyp, expTyp)) then
                  (
                    (*
                    printDatatype(varTyp);
                    printDatatype(expTyp);
                    *)
                    ErrorMsg.error pos "AssignExp lvalue type and exp type don't match";
                    {exp=Translate.transError(), ty=Types.BOTTOM}
                  )
                  else ({exp=Translate.assignExp(varExp,initExp), ty=Types.UNIT})
                )

              end
          | Absyn.RecordExp({fields, typ: Symbol.symbol, pos: Absyn.pos} ) =>
              (* look up typ in tenv, get field list, tycheck each field *)
              let
                val found_tup_opt = Symbol.look(tenv, typ)

                val sorted_fields = ListMergeSort.sort (fn(x,y) => Symbol.name(#1 x) > Symbol.name(#1 y)) fields
                val sorted_sym_exp_type_fields = map (fn x => (#1 x, transExp(venv, tenv, #2 x, level,looplabel))) sorted_fields
                val sorted_exp_fields = map (fn (sym,{exp=treeExp,ty=typ}) => (sym,typ)) sorted_sym_exp_type_fields
                val sorted_sym_treeExp_list = map(fn (sym,{exp=treeExp,ty=typ}) => (sym,treeExp))  sorted_sym_exp_type_fields
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
                        
                        {exp=Translate.recordExp(sorted_sym_treeExp_list), ty = Types.RECORD(get_fields_func, unique_ref)}
                      ) else (
                        ErrorMsg.error pos "Record creation does not assign value to all fields";
                        {exp=Translate.transError(), ty = Types.BOTTOM}
                      )
                    )
                  end
                  )
                | (_) => (
                  ErrorMsg.error pos "Unknown record type instantiated";
                  {exp=Translate.transError(), ty = Types.BOTTOM}
                )
               )
              end
        )

      (* Just handle non-recursive tydecs first *)
      and transDec(venv, tenv, dec, level, looplabel): {venv:Env.enventry Symbol.table, tenv:Types.ty Symbol.table, exp:Translate.exp option} =
        (* exp is option type because TypeDec and FunDec do not have IR representation *) 
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
              {venv=venv, tenv=tenv, exp=NONE}
            )
            else
             (
               let fun foldFn (somety,tenv) =
                case somety of {name=name, ty=ty, pos=pos} =>
                  (
                    Symbol.enter(tenv, name, transTy(name, tenv,ty,tydecList, symTable, unitRefTable, []))
                  )
                val new_tenv = foldl foldFn tenv tydecList
               in (
                map (fn x => let
                                val sym = (#name x)
                                val typeOpt = Symbol.look(new_tenv, sym)
                              in
                                case typeOpt of SOME(Types.RECORD(f,_)) => (f(); ())
                                | SOME(Types.ARRAY(f,_)) => (f(); ())
                                | (_) => ()
                              end
                    ) tydecList;
                {venv=venv, tenv=new_tenv, exp=NONE})
               end
            )
          )
        end

        | Absyn.VarDec({name, escape:bool ref, typ, init, pos}) => 
          (* if isSome typ then need to do type checking of init against typ, else just save the type of init for var name in venv*)
           if isSome typ then
            (
            let val expectedType = getTypeOrBottom(tenv, #1 (valOf typ))
            in
              case expectedType of Types.BOTTOM =>
                (
                  ErrorMsg.error pos "Vardec expected type not found.";
                  {venv=venv, tenv=tenv, exp=NONE}
                )
              | (_) =>
                (
                    let 
                      val actual = transExp(venv, tenv, init, level,looplabel)
                      val actualType = (#ty actual)
                      val newAlloc = Translate.allocLocal(level)(!escape)
                      val newVarEntry = Env.VarEntry({access=newAlloc,ty=actualType})
                      val dum = debugPrint("Calling Translate ecp="^Bool.toString(!escape)^" for var "^Symbol.name(name)^".\n",pos)
                    in (
                      (* begin of case *)
                    case (actualType, expectedType) of (Types.ARRAY(sym, reff),Types.ARRAY(sym2, reff2)) => (
                          if reff = reff2 then (
                              let
                                val venv = Symbol.enter(venv, name, newVarEntry)
                                val res = Translate.varDecAlloc(newAlloc, #exp actual)
                              in
                                {venv=venv, tenv=tenv, exp=SOME(res)}
                            end
                          ) else (
                              ErrorMsg.error pos "Different array types, unique refs dont match";
                              {venv=venv, tenv=tenv, exp=NONE}
                          )
                         )
                    | (Types.RECORD(sym, reff),Types.RECORD(sym2, reff2)) => (
                          if reff = reff2 then (
                            let
                                val venv = Symbol.enter(venv, name, newVarEntry)
                                val res = Translate.varDecAlloc(newAlloc, #exp actual)
                            in
                              {venv=venv, tenv=tenv, exp=SOME(res)}
                            end
                          ) else (
                              ErrorMsg.error pos "Different record types, unique refs dont match";
                              {venv=venv, tenv=tenv, exp=NONE}
                          )
                         )
                    | (Types.NIL,Types.RECORD(sym2, reff2)) => (
                      let
                        val thisVarEntry = Env.VarEntry({access=Translate.allocLocal(level)(true),ty=expectedType})
                        val venv = Symbol.enter(venv, name, thisVarEntry)
                        val res = Translate.varDecAlloc(newAlloc, #exp actual) (* actual will be T.CONST 0 *)
                      in
                        {venv=venv, tenv=tenv, exp=SOME(res)}
                      end
                    )
                    | (x, y) => (
                      if tyEqualTo(actualType, expectedType)
                      then
                        let
                          val venv = Symbol.enter(venv, name, newVarEntry)
                          val res = Translate.varDecAlloc(newAlloc, #exp actual)
                        in
                          {venv=venv, tenv=tenv, exp=SOME(res)}
                        end
                      else  (
                        ErrorMsg.error pos "Declared type does not match exp type.";
                        {venv=venv, tenv=tenv, exp=NONE}
                      )
                    )
                     (* end of case *)
                  ) (* end in *)
              end
             ) (* end else *)
             end

             ) else (
              (* typ is NONE, so nothing to check, just put actualType of init in table *)
              let 
                val actual = transExp(venv, tenv, init, level,looplabel)
                val actualType = (#ty actual)
                val newAlloc = Translate.allocLocal(level)(!escape)
                val newVarEntry = Env.VarEntry({access=newAlloc,ty=actualType})
                val dum = debugPrint("Calling Translate ecp="^Bool.toString(!escape)^" for var "^Symbol.name(name)^".\n",pos)
                val res = Translate.varDecAlloc(newAlloc, #exp actual)
              in
                case actualType of 
                  Types.NIL => (
                      ErrorMsg.error pos "Assign nil to new var\n";
                      {venv=venv,tenv=tenv,exp=SOME(res)}
                    )
                  | (_) => {venv=Symbol.enter(venv, name, newVarEntry), tenv=tenv, exp=SOME(res)}
                
              end
            )
            
        | Absyn.FunctionDec(fundeclist) =>
          (* add function headers to venv *)
          (* foldl with transExp on body; returns boolean *)
          let
            val venv = venvWithFunctionHeaders(venv, fundeclist, tenv, level)
            val dupErr = #2 (foldl checkFunDupsFoldFunc ([], 0) fundeclist)
            (**** START OF foldFn ****)
            fun foldFn (fundec, bool) =
            case bool of
              false => false
            | true =>
              (case fundec of
                {name: Absyn.symbol, params:{name:Symbol.symbol,escape: bool ref, typ: Symbol.symbol, pos:Absyn.pos} list , result: (Symbol.symbol * Absyn.pos) option, body: Absyn.exp, pos: Absyn.pos} =>
                  let
                    (* add parameters to venv *)
                    val funLabel = Temp.newlabel()
                    val funLevel = Translate.newLevel({parent=level,name=funLabel,formals=paramsToFormals(params)})
                    val venv = addFunctionParamsVenv(venv,tenv,params,funLevel)
                    val actualReturn =  transExp(venv,tenv,body,funLevel,looplabel)
                    val actualReturnType = #ty actualReturn 
                    val expectedReturnType =
                      let
                        val expectedReturnTypeOpt = Symbol.look(venv, name)
                      in
                        case expectedReturnTypeOpt of
                          SOME(Env.FunEntry({label=lab, level=lev, formals=formals,result=result})) => result
                          | SOME(_) => (ErrorMsg.error pos "Expected function but got something else in transDec."; Types.BOTTOM)
                          | NONE => (ErrorMsg.error pos "Undefined function in transDec."; Types.BOTTOM)
                      end
                  in (
                    (* Translate.funDec run purely for side effect of adding PROC to fraglist *)
                    Translate.funDec(funLevel,funLabel,#exp actualReturn);
                    
                    (* DEBUG PRINTS START
                    print("Type of expected return type:\n");
                    printDatatype(expectedReturnType);
                    print("\nType of actual return:\n");
                    printDatatype(actualReturnType);
                    print("\n");
                    DEBUG PRINTS END *)
                    
                    if tyEqualTo(expectedReturnType, actualReturnType) then true
                    else (ErrorMsg.error pos "Function return type mismatch.";false)
                    )
                  end
              )
            (**** END OF foldFn ****)
            val foldAns = foldl foldFn true fundeclist 
          in (
            if foldAns <> true andalso foldAns <> false then print("wut?") else ();
            case (dupErr, foldAns) of 
              (1,_) => {venv=venv,tenv=tenv,exp=NONE}
            | (0, true) => {venv=venv,tenv=tenv,exp=NONE} 
            | (0, false) => (ErrorMsg.error 0 "Fundecs do not type check\n"; {venv=venv,tenv=tenv,exp=NONE})
            | _ => (ErrorMsg.error 0 "GGXX dupErr is not 0 or 1"; print(((Int.toString dupErr)^"asfdafad"));{venv=venv,tenv=tenv,exp=NONE})
            )
          end

  and transTy(name, tenv, Absyn.RecordTy(fieldlist:{escape:bool ref, name:Symbol.symbol, pos:Absyn.pos,
                    typ:Symbol.symbol} list), decList, symTable ,unitRefList, currentPath: Symbol.symbol list): Types.ty =
    let
      fun get_fields () =
        (
        map (fn {name,escape,typ,pos} =>
        let val nextTypOpt = getTyOfName(decList, typ)
          val foundInTenvOpt = Symbol.look(tenv, typ)
          in
            if isSome nextTypOpt then
              (name, transTy(typ, tenv, #ty (valOf nextTypOpt), decList, symTable, unitRefList, []))
            else
              if isSome foundInTenvOpt then (name, (valOf foundInTenvOpt)) else (
              ErrorMsg.error pos "[GET FIELDS] Symbol is not found in local tenv or decList";
              (name, Types.BOTTOM))
          end
        ) (ListMergeSort.sort (fn(x,y) => Symbol.name(#name x) > Symbol.name(#name y)) fieldlist)
        )
    in (
      (*
      print("Transty parsing record type: " ^ Symbol.name(name) ^ "\n");
      *)
      let val unitRef =
        let val unitRefOpt = Symbol.look(unitRefList, name) in
          case unitRefOpt of SOME(x) => x
          | NONE => (ErrorMsg.error 0 "Unit ref not found for record type.";
          ref())
        end
      in
        Types.RECORD (get_fields,unitRef)
      end
      )
    end

    | transTy(name, tenv, Absyn.NameTy(ty_sym, pos), decList, symTable ,unitRefList, currentPath: Symbol.symbol list): Types.ty =
    let
      val updatedPath = name :: currentPath
    in
    (
      (* get fields pointed to by current record from sym table*)
      let
        val foundOpt = List.find (fn x => x = ty_sym) updatedPath
          in
            case foundOpt of SOME(x) =>
              (* Cycle with no record type detected, we know this because we do not add record types to list *)
              (* Error, illegal cycle*)
              (
              ErrorMsg.error pos "Illegal cycle in type declaration!";
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
      let
      fun get_fields () =
        let
          val tySymOpt = getTyOfName (decList, ty_sym)
          val foundInTenvOpt = Symbol.look(tenv, ty_sym)
        in
          case tySymOpt of SOME(tySym) => transTy(ty_sym, tenv, (#ty tySym), decList, symTable ,unitRefList, [])
          | NONE => (
            case foundInTenvOpt of SOME(typ) => typ
            | NONE => (ErrorMsg.error pos "Type not found in transTy for ArrayTy."; Types.BOTTOM)
          )
        end
    in (
      (*
      print("parsing array type: " ^ Symbol.name(name) ^ "\n");
      *)
      let val unitRef =
        let val unitRefOpt = Symbol.look(unitRefList, name) in
          case unitRefOpt of SOME(x) => x
          | NONE => (ErrorMsg.error 0 "Unit ref not found for record type.";
          ref())
        end
      in
        Types.ARRAY(get_fields,unitRef)
      end
      )
    end


  and transVar(venv:venv, tenv:tenv, var:Absyn.var, level:Translate.level,looplabel:Temp.label) =
    (*pattern match on Absyn.var*)
    case var of Absyn.SimpleVar(sym, pos) =>
      let val varTypOpt = Symbol.look(venv, sym)
      in
        (if isSome varTypOpt
        then (
        let val varTyp = valOf varTypOpt
          in
            case varTyp of
              Env.VarEntry({access=acc, ty=typ}) => {exp=Translate.simpleVar(acc,level), ty=typ}
            | _ => (
                    ErrorMsg.error pos "using function as a var";
                    {exp=Translate.transError(), ty=Types.BOTTOM}
                   )
          end
        )
        else
          (
           ErrorMsg.error pos "var was not declared";
           {exp=Translate.transError(), ty=Types.BOTTOM}
          )
        )
      end

      | Absyn.FieldVar(var, symbol, pos) =>
      let
        val {exp=varExp, ty=varType} = transVar(venv,tenv,var,level,looplabel)
      in
        if not (tyEqualTo(varType, Types.RECORD((fn()=>[]), ref())))
        then (
          ErrorMsg.error pos "accessing field of non-fieldVar\n";
          {exp=Translate.transError(), ty=Types.BOTTOM}
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
                    {exp = Translate.transError(), ty = Types.BOTTOM}
                  )
                  | SOME (sym,typ) => (
                    (* find idx of symobol in sorted field list *)
                    let
                      fun indexFoldFn ((x,xType),(idx,currIdx)) = if x = sym then(currIdx, currIdx+1) else (idx, currIdx + 1)
                      val idx = #1(foldl indexFoldFn (0,0) fieldList) 
                    in  
                      {exp=Translate.fieldVar(varExp,idx),ty=typ}
                    end
                  )
              )
              end
            | (_) => (ErrorMsg.error pos "Non-record base variable for FieldVar"; {exp=Translate.transError(), ty=Types.BOTTOM})
        )
      end

    | Absyn.SubscriptVar(var, exp, pos) =>
      (* check var is array *)
      let
          val {exp=subscriptExp,ty=expType} = transExp(venv:venv,tenv:tenv,exp,level,looplabel)
          val {exp=varExp,ty=varType} = transVar(venv:venv,tenv:tenv,var,level,looplabel)
      in (
         if not (tyEqualTo(expType,Types.INT))
         then (
            ErrorMsg.error pos "Non-integer used as array index.\n";
           {exp=Translate.transError(), ty=Types.BOTTOM}
         )
         else (
            case varType of
                (* check exp is int *)
                Types.ARRAY(arrayGetterFn, someref) => {exp=Translate.subscriptVar(varExp,subscriptExp),ty=arrayGetterFn()}
              | _ => (
                ErrorMsg.error pos "subscript var on nonarray type.\n";
                {exp=Translate.transError(), ty=Types.BOTTOM}
              )
          )
      )
      end
      
  (* end all mutually recursive function defs and start in block of transProg *)
  val {exp=exp, ty=ty} = transExp(venv,tenv, e, Translate.outermost, Temp.newlabel()); (* you can't break in outer loop so use random label *)
  in
  (
    Translate.funDec(Translate.outermost, Temp.newlabel(), exp);
    Translate.getResult()
  )
  end
end
