LABEL L17
MOVE(
 TEMP t136,
 MEM(
  TEMP t100:FP))
MOVE(
 TEMP t127:intExpTemp,
 CONST 10)
MOVE(
 TEMP t135,
 BINOP(PLUS,
  TEMP t127:intExpTemp,
  CONST 1))
MOVE(
 TEMP t128:intExpTemp,
 CONST 0)
EXP(
 CALL(
  NAME L12:initArray,
   TEMP t136,
   TEMP t135,
   TEMP t128:intExpTemp))
MOVE(
 TEMP t137,
 TEMP t102:RV)
MOVE(
 TEMP t127:intExpTemp,
 CONST 10)
MOVE(
 MEM(
  TEMP t137),
 TEMP t127:intExpTemp)
MOVE(
 TEMP t130:localVarTemp,
 BINOP(PLUS,
  TEMP t102:RV,
  CONST 4))
MOVE(
 TEMP t132:arraySizeTemp,
 MEM(
  BINOP(MINUS,
   TEMP t130:localVarTemp,
   CONST 4)))
MOVE(
 TEMP t138,
 TEMP t132:arraySizeTemp)
MOVE(
 TEMP t131:intExpTemp,
 CONST 0)
CJUMP(GT,
 TEMP t138,
 TEMP t131:intExpTemp,
 L14:array idx ok label,L13:array idx out of bound label)
LABEL L13:array idx out of bound label
EXP(
 CALL(
  NAME L16:exit,
   CONST 1))
MOVE(
 TEMP t133:subscriptVarResultTemp,
 CONST 1)
LABEL L15:subscriptVar done label
MOVE(
 TEMP t140,
 TEMP t130:localVarTemp)
MOVE(
 TEMP t131:intExpTemp,
 CONST 0)
MOVE(
 TEMP t141,
 BINOP(PLUS,
  TEMP t140,
  BINOP(MUL,
   TEMP t131:intExpTemp,
   CONST 4)))
MOVE(
 TEMP t134:intExpTemp,
 CONST 1)
MOVE(
 MEM(
  TEMP t141),
 TEMP t134:intExpTemp)
MOVE(
 TEMP t102:RV,
 CONST 0)
JUMP(
 NAME L18:DONE)
LABEL L14:array idx ok label
MOVE(
 TEMP t139,
 TEMP t130:localVarTemp)
MOVE(
 TEMP t131:intExpTemp,
 CONST 0)
MOVE(
 TEMP t133:subscriptVarResultTemp,
 MEM(
  BINOP(PLUS,
   TEMP t139,
   BINOP(MUL,
    TEMP t131:intExpTemp,
    CONST 4))))
JUMP(
 NAME L15:subscriptVar done label)
LABEL L18:DONE
L17:
lw t142:T.MEM(e1), 0(t100:FP)
add t136, $zero, t142:T.MEM(e1) 
li t127:intExpTemp, 10
addi t143:T.BINOP(T.PLUS, e1, T.CONST i), t127:intExpTemp, 1
add t135, $zero, t143:T.BINOP(T.PLUS, e1, T.CONST i) 
li t128:intExpTemp, 0
move t105:a0,t136 
move t106:a1,t135 
move t107:a2,t128:intExpTemp 
addi t144:T.CONST i, $zero,16  
sub t145:T.BINOP(oper, e1, e2), t101:SP, t144:T.CONST i
add t101:SP, $zero, t145:T.BINOP(oper, e1, e2) 
jal L12:initArray
add t137, $zero, t102:RV 
li t127:intExpTemp, 10
sw t127:intExpTemp, 0(t137)
addi t146:T.BINOP(T.PLUS, e1, T.CONST i), t102:RV, 4
add t130:localVarTemp, $zero, t146:T.BINOP(T.PLUS, e1, T.CONST i) 
addi t148:T.CONST i, $zero,4  
sub t149:T.BINOP(oper, e1, e2), t130:localVarTemp, t148:T.CONST i
lw t147:T.MEM(e1), 0(t149:T.BINOP(oper, e1, e2))
add t132:arraySizeTemp, $zero, t147:T.MEM(e1) 
add t138, $zero, t132:arraySizeTemp 
li t131:intExpTemp, 0
sub t150:comp_diff, t138, t131:intExpTemp 
bgtz t150:comp_diff, L14:array idx ok label
L13:array idx out of bound label:
addi t151:T.CONST i, $zero,1  
move t105:a0,t151:T.CONST i 
addi t152:T.CONST i, $zero,16  
sub t153:T.BINOP(oper, e1, e2), t101:SP, t152:T.CONST i
add t101:SP, $zero, t153:T.BINOP(oper, e1, e2) 
jal L16:exit
li t133:subscriptVarResultTemp, 1
L15:subscriptVar done label:
add t140, $zero, t130:localVarTemp 
li t131:intExpTemp, 0
addi t154:T.CONST i, $zero,4  
mult t155:T.BINOP(oper, e1, e2), t131:intExpTemp, t154:T.CONST i
add t156:T.BINOP(oper, e1, e2), t140, t155:T.BINOP(oper, e1, e2)
add t141, $zero, t156:T.BINOP(oper, e1, e2) 
li t134:intExpTemp, 1
sw t134:intExpTemp, 0(t141)
li t102:RV, 0
j L18:DONE
L14:array idx ok label:
add t139, $zero, t130:localVarTemp 
li t131:intExpTemp, 0
addi t158:T.CONST i, $zero,4  
mult t159:T.BINOP(oper, e1, e2), t131:intExpTemp, t158:T.CONST i
add t160:T.BINOP(oper, e1, e2), t139, t159:T.BINOP(oper, e1, e2)
lw t157:T.MEM(e1), 0(t160:T.BINOP(oper, e1, e2))
add t133:subscriptVarResultTemp, $zero, t157:T.MEM(e1) 
j L15:subscriptVar done label
L18:DONE:
