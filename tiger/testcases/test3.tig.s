LABEL L15
MOVE(
 TEMP tt128,
 CALL(
  NAME L13:malloc,
   MEM(
    TEMP tt100:FP),
   CONST 8))
MOVE(
 TEMP tt130,
 BINOP(PLUS,
  TEMP tt128,
  BINOP(MUL,
   CONST 0,
   CONST 4)))
MOVE(
 TEMP tt127,
 CONST 1000)
MOVE(
 MEM(
  TEMP tt130),
 TEMP tt127)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP tt128,
   BINOP(MUL,
    CONST 1,
    CONST 4))),
 NAME L12)
MOVE(
 TEMP tt129,
 TEMP tt128)
MOVE(
 MEM(
  BINOP(PLUS,
   BINOP(MUL,
    CONST 1,
    CONST 4),
   TEMP tt129)),
 NAME L14)
MOVE(
 TEMP tt102:RV,
 TEMP tt129)
JUMP(
 NAME L16)
LABEL L16
L15:
lw t131, 0(t100:FP)
move t105:a0,t131 
addi t132, $zero,8  
move t106:a1,t132 
addi t133, $zero,16  
sub t134, t101:SP, t133
add t101:SP, $zero, t134 
jal L13:malloc
add t128, $zero, t102:RV 
addi t135, $zero,0  
addi t136, $zero,4  
mult t137, t135, t136
add t138, t128, t137
add t130, $zero, t138 
li t127, 1000
sw t127, 0(t130)
addi t139, $zero,1  
addi t140, $zero,4  
mult t141, t139, t140
add t142, t128, t141
la t143, L12
sw t143, 0(t142)
add t129, $zero, t128 
addi t144, $zero,1  
addi t145, $zero,4  
mult t146, t144, t145
add t147, t146, t129
la t148, L14
sw t148, 0(t147)
add t102:RV, $zero, t129 
j L16
L16:
L14: .ascii "Somebody"
L12: .ascii "Nobody"
