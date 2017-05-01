LABEL L16
MOVE(
 TEMP tt134,
 MEM(
  TEMP tt100:FP))
MOVE(
 TEMP tt133,
 CONST 10)
MOVE(
 TEMP tt102:RV,
 CALL(
  NAME L12,
   TEMP tt134,
   TEMP tt133))
JUMP(
 NAME L17)
LABEL L17
L16:
lw $s0, 0($FP)
move $s0, $s0 
li $s1, 10
move $a0,$s0 
move $a1,$s1 
addi $s0, $zero,16  
sub $s0, $SP, $s0
move $SP, $s0 
jal L12
move $RV, $RV 
j L17
L17:
LABEL L12
MOVE(
 TEMP tt138,
 TEMP tt128)
MOVE(
 TEMP tt129,
 CONST 0)
CJUMP(EQ,
 TEMP tt138,
 TEMP tt129,
 L13,L14)
LABEL L14
MOVE(
 TEMP tt142,
 TEMP tt128)
MOVE(
 TEMP tt141,
 MEM(
  TEMP tt100:FP))
MOVE(
 TEMP tt140,
 TEMP tt128)
MOVE(
 TEMP tt131,
 CONST 1)
MOVE(
 TEMP tt139,
 CALL(
  NAME L12,
   TEMP tt141,
   BINOP(MINUS,
    TEMP tt140,
    TEMP tt131)))
MOVE(
 TEMP tt132,
 BINOP(MUL,
  TEMP tt142,
  TEMP tt139))
LABEL L15
MOVE(
 TEMP tt102:RV,
 TEMP tt132)
JUMP(
 NAME L18)
LABEL L13
MOVE(
 TEMP tt130,
 CONST 1)
MOVE(
 TEMP tt132,
 TEMP tt130)
JUMP(
 NAME L15)
LABEL L18
L12:
move $s0, $s1 
li $s2, 0
beq $s0, $s2, L13 
L14:
move $s0, $s1 
lw $s2, 0($FP)
move $s2, $s2 
move $s3, $s1 
li $s4, 1
move $a0,$s2 
sub $s2, $s3, $s4
move $a1,$s2 
addi $s2, $zero,16  
sub $s2, $SP, $s2
move $SP, $s2 
jal L12
move $s1, $RV 
mult $s0, $s0, $s1
move $s0, $s0 
L15:
move $RV, $s0 
j L18
L13:
li $s0, 1
move $s0, $s0 
j L15
L18:
