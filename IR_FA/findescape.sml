structure FindEscape: sig val findEscape: Absyn.exp -> unit end =
struct
  type depth = int
  type escEnv = (depth * bool ref) Symbol.table

  val debug = false

  fun debugPrint(msg:string, pos:int) =
    if debug
    then ErrorMsg.error pos msg
    else ()
  fun traverseVar(env:escEnv, d:depth, s:Absyn.var): unit =
    (* look for symbols in escEnv, set to true (env and absyn's bool ref ) if depth is different *)
    (* unwrap to simple var and look up in esc table *)
    case s of
      Absyn.SimpleVar(symbol,pos) => (
        let
          val symLookup = Symbol.look(env,symbol)
        in
          case symLookup of
            SOME(declaredDepth,boolref) => (
                if declaredDepth < d
                then (
                  boolref := true;
                  debugPrint("Set escape of "^Symbol.name(symbol)^" to true. Var declared depth="^Int.toString(declaredDepth)^" var referenced depth="^Int.toString(d)^".\n",pos)
                  )
                else (
                  debugPrint("Set escape of "^Symbol.name(symbol)^" to false. Var declared depth="^Int.toString(declaredDepth)^" var referenced depth="^Int.toString(d)^".\n",pos)

                  )
            )
          | NONE => debugPrint("[ FindEscape ] Symbol "^Symbol.name(symbol)^" not found in scope depth "^Int.toString(d)^".\n",pos)
        end
      )
    | Absyn.FieldVar(var,symbol,pos) => traverseVar(env,d,var)
    | Absyn.SubscriptVar(var,exp,pos) => (
        traverseVar(env,d,var);
        traverseExp(env,d,exp)
      )

  and traverseExp(env:escEnv, d:depth, s:Absyn.exp): unit =
      case s of
        Absyn.VarExp(var) => traverseVar(env,d,var)
      | Absyn.CallExp({func=funSym,args=argList,pos=pos}) => (
          (* Check params:exp list *)
          let
            fun foldArgsFn (argexp, dummy) =
              traverseExp(env,d,argexp)
          in
            foldl foldArgsFn () argList
          end
        )
      | Absyn.OpExp({left=expleft, right=expright, oper=oper, pos=pos}) => (
        (* check leftexp and rightexp *)
        traverseExp(env,d,expleft);
        traverseExp(env,d,expright)
        )
      | Absyn.RecordExp({fields=fieldList,typ=typ,pos=pos}) => (
        (* fieldlist: (symbol, exp, pos) list, check each exp *)
        let
          fun foldFieldFn (field,dummy) =
            case field of
              (symbol,exp,pos) => (
                traverseExp(env,d,exp)
              )
        in
          foldl foldFieldFn () fieldList
        end
        )
      | Absyn.SeqExp(expPosList) => (
        (* fold on exp list, check each exp *)
        let
          fun foldSeqExpFn (exppos,dum) =
            case exppos of
              (exp,pos) => traverseExp(env,d,exp)
        in
          foldl foldSeqExpFn () expPosList
        end
        )
      | Absyn.AssignExp({var=var,exp=exp,pos=pos}) => (
        (* traverseVar on var, traverseExp on exp *)
        traverseVar(env,d,var);
        traverseExp(env,d,exp)
        )
      | Absyn.IfExp({test=testexp, then'=thenexp, else'=elseexpOpt,pos=pos}) => (
        (* check testexp, thenexp, thenexp, elseexp,*)
        traverseExp(env,d,testexp);
        traverseExp(env,d,thenexp);
        case elseexpOpt of
          SOME(elseExp) => traverseExp(env,d,elseExp)
        | NONE => ()
        )
      | Absyn.WhileExp({test=testexp, body=bodyexp, pos=pos}) => (
        (* check testexp, bodyexp *)
        traverseExp(env,d,testexp);
        traverseExp(env,d,bodyexp)
        )
      | Absyn.LetExp({decs=decList, body=bodyexp, pos=pos}) => (
        (* traverseDec on decList, use new env to check bodyexp *)
        let
          val letDepth = d+1
          val letBodyEnv = traverseDecs(env,letDepth,decList)
        in
          traverseExp(letBodyEnv,letDepth,bodyexp)
        end
        )
      | Absyn.ArrayExp({typ=typ,size=sizeExp,init=initExp, pos=pos}) => (
        (* check sizeExp, initExp *)
        traverseExp(env,d,sizeExp);
        traverseExp(env,d,initExp)
        )
      | Absyn.ForExp({var=varSym,escape=ecpRef,lo=loExp,hi=hiExp,body=bodyExp,pos=pos}) => (
          let
            val forDepth = d+1
            val forEnv = Symbol.enter(env,varSym,(forDepth,ecpRef))
          in
            ecpRef := false;
            traverseExp(env,d,loExp);
            traverseExp(env,d,hiExp);
            traverseExp(forEnv,forDepth,bodyExp)
          end
        )

      | (_) => ()
  and traverseDecs(env, d, s: Absyn.dec list): escEnv =
    (* parse var decs, add to env with depth d, add dec's bool ref to env, set ref to false *)
    let
      fun foldDecsFn (dec,envVar) =
        case dec of
          Absyn.VarDec({name=nameSym,escape=ecpRef,typ=typ,init=initExp,pos=pos}) => (
              (* check initExp using env passed in; add nameSym & depth & ecpRef to create new env *)
              let
                val newEnv = Symbol.enter(envVar,nameSym,(d,ecpRef))
              in
                traverseExp(envVar,d,initExp);
                ecpRef := false;
                newEnv
              end
          )
        | Absyn.FunctionDec(fundecList) => (
            (* check function body exp on env *)
            let
              fun foldFundecFn (fundec, envNotChanged) =
                case fundec of
                  {name=name,params=params,result=result,body=bodyexp,pos=pos} => (
                      let
                        fun foldFnParamsFn (field, paramEnv) =
                          case field of
                            {name=namesym,escape=ecpRef,typ=typ,pos=pos} => (
                                Symbol.enter(paramEnv,namesym,(d+1,ecpRef))
                            )
                      in
                        traverseExp(foldl foldFnParamsFn envNotChanged params,d+1,bodyexp);
                        envNotChanged
                      end
                  )
            in
              foldl foldFundecFn envVar fundecList
            end
          )
        | (_) => envVar
    in
      foldl foldDecsFn env s
    end

  fun findEscape(prog:Absyn.exp):unit =
      traverseExp(Symbol.empty, 0, prog)

end
