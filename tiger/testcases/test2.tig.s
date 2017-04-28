LABEL L13
MOVE(
 TEMP tt132,
 MEM(
  TEMP tt100:FP))
MOVE(
 TEMP tt127:intExpTemp,
 CONST 10)
MOVE(
 TEMP tt131,
 BINOP(PLUS,
  TEMP tt127:intExpTemp,
  CONST 1))
MOVE(
 TEMP tt128:intExpTemp,
 CONST 0)
EXP(
 CALL(
  NAME L12:initArray,
   TEMP tt132,
   TEMP tt131,
   TEMP tt128:intExpTemp))
MOVE(
 TEMP tt133,
 TEMP tt102:RV)
MOVE(
 TEMP tt127:intExpTemp,
 CONST 10)
MOVE(
 MEM(
  TEMP tt133),
 TEMP tt127:intExpTemp)
MOVE(
 TEMP tt130:localVarTemp,
 BINOP(PLUS,
  TEMP tt102:RV,
  CONST 4))
MOVE(
 TEMP tt102:RV,
 TEMP tt130:localVarTemp)
JUMP(
 NAME L14)
LABEL L14
L13:
lw t134:T.MEM(e1), 0(t100:FP)
add t132, $zero, t134:T.MEM(e1) 
li t127:intExpTemp, 10
addi t135:T.BINOP(T.PLUS, e1, T.CONST i), t127:intExpTemp, 1
add t131, $zero, t135:T.BINOP(T.PLUS, e1, T.CONST i) 
li t128:intExpTemp, 0
move t105:a0,t132 
move t106:a1,t131 
move t107:a2,t128:intExpTemp 
addi t136:T.CONST i, $zero,16  
sub t137:T.BINOP(oper, e1, e2), t101:SP, t136:T.CONST i
add t101:SP, $zero, t137:T.BINOP(oper, e1, e2) 
jal L12:initArray
add t133, $zero, t102:RV 
li t127:intExpTemp, 10
sw t127:intExpTemp, 0(t133)
addi t138:T.BINOP(T.PLUS, e1, T.CONST i), t102:RV, 4
add t130:localVarTemp, $zero, t138:T.BINOP(T.PLUS, e1, T.CONST i) 
add t102:RV, $zero, t130:localVarTemp 
j L14
L14:
