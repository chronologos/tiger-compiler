## NOTES
#strExp          stringExp(strExp:A.StringExp) =
#strCmp          strcmp(str1:exp,str2:exp,oper:A.oper,callLevel:level):Cx = 
# nil              fun nilExp():T.exp =
# int              intExp(intExp:Absyn.IntExp) =
# opexp           opExp(leftExp:exp, rightExp:exp, oper:Absyn.oper) : exp =
# assignexp         fun assignExp(varExp:exp,assignExp:exp) = 
# simplevar       fun simpleVar(a:access,l:level): exp =
# subscriptvar          fun subscriptVar(varAccess:exp, offsetExp:exp) = 
# fieldvar           fun fieldVar(varAccess:exp, fieldOffset:int) =  (* fieldOffset as of in sorted field list *)
# seqExp            fun seqExp(l:exp list) = 
# recordexp         fun recordExp(fieldlist:(Symbol.symbol, exp) list) = 
# arrayexp          fun arrayExp(sizeExp:exp,initExp:exp,cLevel:level) =
# callExp         callExp(dLevel:level, cLevel:level, lab:Temp.label, expList:exp list) = 
# ifExp             fun transIf({text=e1, then'=Nx(i2), else'=SOME(Nx(i3)), pos=_}) =
# letexp             fun letExp(decExpList:exp list, body:exp) = 
# functiondec       fun funDec(funLevel:level, lab:Temp.label, body:exp):Frame.PROC = 
# vardec          fun varDecAlloc(varAccess:access,initExp:exp) = 
# whileexp          fun transWhile({test=test, body=body, pos=_}, donelabel) = 
# forexp            fun transFor({var=var, escape=escape, lo=lo, hi=hi, body=body, pos=_}, loopVar) 
# breakexp        fun transBreak(donelabel) = T.JUMP(T.NAME(donelabel), [donelabel])


# TODO
bounds checking
possibly convert tree.sml to safe alternative
instruction to move RV to register on function exit [done?]
test 6: in which direction is memory read and written ie. MEM[-4] means -4 to -8 or 0 to -4?
