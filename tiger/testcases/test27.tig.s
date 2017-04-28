LABEL L13
MOVE(
 TEMP tt128,
 CONST 0)
MOVE(
 TEMP tt129,
 TEMP tt128)
MOVE(
 TEMP tt131,
 MEM(
  TEMP tt100:FP))
MOVE(
 TEMP tt130,
 CONST 2)
MOVE(
 TEMP tt102:RV,
 CALL(
  NAME L12,
   TEMP tt131,
   TEMP tt130))
JUMP(
 NAME L14)
LABEL L14
L13:
li $s0, 0
move $s0, $s0 
lw $s0, 0($FP)
move $s0, $s0 
li $s1, 2
move $a0,$s0 
move $a1,$s1 
addi $s0, $zero,16  
sub $s0, $SP, $s0
move $SP, $s0 
jal L12
move $RV, $RV 
j L14
L14:
LABEL L12
MOVE(
 TEMP tt102:RV,
 TEMP tt106:a0)
JUMP(
 NAME L15)
LABEL L15
L12:
move $RV, $a0 
j L15
L15:
