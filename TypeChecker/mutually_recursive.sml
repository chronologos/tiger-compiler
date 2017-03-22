transTy (tenv, A.RecordTy(fields), A.TypeDec(declist)) =
    let
        fun get_fields () = 
                (* define recursive/DFS with visited list search function that returns base type given a typedec and a type_symbol to search for *)
                fun R(A.TypeDec(declist), ty_symbol:Symbol.symbol, visited) = 
                case declist of x::xs => if #name x = ty_symbol then ?? else R(xs, symbol, (#name x) :: visited)
                | [x] =>
                | [] =>
                
                (* add all tydecs from A.TypeDec to localtenv using fold*)
                in

                    map (fn {name,escape,typ,pos} =>
			                  case S.look(tenv,typ) of
			                      SOME(ty) => (name,ty)
			                    | NONE => case S.look(localtenv, typ) of
                              SOME(ty) => (name,ty)
                              | NONE => error!
			              ) fields
    in
        get_fields()
        T.RECORD (get_fields, ref ())
    end


-----------

transTy (tenv, A.RecordTy(fields), A.TypeDec(declist)) =
    let
        fun get_fields () = 
            let 
                val localtenv = Symbol.empty 
                fun add_to_tenv ((symbol, NameTy(ty_symbol, pos)), symboltable) = 
                    if isSome Symbol.look(symboltable, symbol)
                    then (
                        ErrorMsg.error 0 "Declared two types of the same symbol in the same tydec";
                        some_tenv;
                    )
                    else (
                        type = Symbol.look(tenv, symbol);
                        case type of SOME(x) => 
                        | NONE => (ErrorMsg.error 0 "No such type"; Types.NIL)
                    Symbol.enter(symboltable, symbol, ty_symbol)
                    
                    )
                | add_to_tenv ((symbol, ArrayTy(ty_symbol, pos)), symboltable)
                    if isSome Symbol.look(symboltable, symbol)
                    then (
                        ErrorMsg.error 0 "Declared two types of the same symbol in the same tydec";
                        some_tenv;
                    )
                    else Symbol.enter(symboltable, symbol, ty_symbol)
                | add_to_tenv ((symbol, ArrayTy(field_list)), symboltable)
                if isSome Symbol.look(symboltable, symbol)
                    then (
                        ErrorMsg.error 0 "Declared two types of the same symbol in the same tydec";
                        some_tenv;
                    )
                else Symbol.enter(symboltable, symbol, ty_symbol)
            in
                foldl add_to_tenv, Symbol.empty, declist
                    (* add all tydecs from A.TypeDec to localtenv if they are record type using fold*)
                    map(
                    
                        fn (name, ty, pos) => S.enter(localtenv, name, T.RECORD(get_fields, ref() ))),
                        declist
                        )
                    (* above code is wrong change to fold *)
                    map (fn {name,escape,typ,pos} =>
			                  case S.look(tenv,typ) of
			                      SOME(ty) => (name,ty)
			                    | NONE => case S.look(localtenv, typ) of
                              SOME(ty) => (name,ty)
                              | NONE => error!
			              ) fields
    in
        T.RECORD (get_fields, ref ())
    end
