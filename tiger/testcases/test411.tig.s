LABEL L16
MOVE(
 TEMP tt133,
 MEM(
  TEMP tt100:FP))
MOVE(
 TEMP tt132,
 CONST 10)
MOVE(
 TEMP tt102:RV,
 CALL(
  NAME L12,
   TEMP tt133,
   TEMP tt132))
JUMP(
 NAME L17)
LABEL L17
L16:
lw t134, 0(t100:FP)
add t133, $zero, t134 
li t132, 10
move t105:a0,t133 
move t106:a1,t132 
addi t135, $zero,16  
sub t136, t101:SP, t135
add t101:SP, $zero, t136 
jal L12
add t102:RV, $zero, t102:RV 
j L17
L17:
LABEL L12
MOVE(
 TEMP tt137,
 TEMP tt127)
MOVE(
 TEMP tt128,
 CONST 0)
CJUMP(EQ,
 TEMP tt137,
 TEMP tt128,
 L13,L14)
LABEL L14
MOVE(
 TEMP tt141,
 TEMP tt127)
MOVE(
 TEMP tt140,
 MEM(
  TEMP tt100:FP))
MOVE(
 TEMP tt139,
 TEMP tt127)
MOVE(
 TEMP tt130,
 CONST 1)
MOVE(
 TEMP tt138,
 CALL(
  NAME L12,
   TEMP tt140,
   BINOP(MINUS,
    TEMP tt139,
    TEMP tt130)))
MOVE(
 TEMP tt131,
 BINOP(MUL,
  TEMP tt141,
  TEMP tt138))
LABEL L15
MOVE(
 TEMP tt102:RV,
 TEMP tt131)
JUMP(
 NAME L18)
LABEL L13
MOVE(
 TEMP tt129,
 CONST 1)
MOVE(
 TEMP tt131,
 TEMP tt129)
JUMP(
 NAME L15)
LABEL L18
L12:
add t137, $zero, t127 
li t128, 0
beq t137, t128, L13 
L14:
add t141, $zero, t127 
lw t143, 0(t100:FP)
add t140, $zero, t143 
add t139, $zero, t127 
li t130, 1
move t105:a0,t140 
sub t144, t139, t130
move t106:a1,t144 
addi t145, $zero,16  
sub t146, t101:SP, t145
add t101:SP, $zero, t146 
jal L12
add t138, $zero, t102:RV 
mult t147, t141, t138
add t131, $zero, t147 
L15:
add t102:RV, $zero, t131 
j L18
L13:
li t129, 1
add t131, $zero, t129 
j L15
L18:
