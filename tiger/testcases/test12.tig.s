LABEL L15
MOVE(
 TEMP tt128,
 CONST 0)
MOVE(
 TEMP tt129,
 TEMP tt128)
MOVE(
 TEMP tt130,
 CONST 0)
MOVE(
 TEMP tt134,
 TEMP tt130)
MOVE(
 TEMP tt131,
 CONST 100)
MOVE(
 TEMP tt135,
 TEMP tt131)
CJUMP(LE,
 TEMP tt134,
 TEMP tt135,
 L14:for_true_branch,L12)
LABEL L12
MOVE(
 TEMP tt102:RV,
 CONST 0)
JUMP(
 NAME L16)
LABEL L13:for_true_init_branch
MOVE(
 TEMP tt134,
 BINOP(PLUS,
  TEMP tt134,
  CONST 1))
LABEL L14:for_true_branch
EXP(
 TEMP tt133)
CJUMP(LT,
 TEMP tt134,
 TEMP tt135,
 L13:for_true_init_branch,L17)
LABEL L17
JUMP(
 NAME L12)
LABEL L16
L15:
li t128, 0
move t129, t128 
li t130, 0
move t134, t130 
li t131, 100
move t135, t131 
sub t136, t134, t135 
blez t136, L14:for_true_branch
L12:
li t102:RV, 0
j L16
L13:for_true_init_branch:
addi t137, t134, 1
move t134, t137 
L14:for_true_branch:
sub t138,t134,t135 
bltz t138, L13:for_true_init_branch
L17:
j L12
L16:
L15:
li $s0, 0
move $s0, $s0 
li $s0, 0
move $s2, $s0 
li $s0, 100
move $s1, $s0 
sub $s0, $s2, $s1 
blez $s0, L14:for_true_branch
L12:
li $RV, 0
j L16
L13:for_true_init_branch:
addi $s0, $s2, 1
move $s2, $s0 
L14:for_true_branch:
sub $s0,$s2,$s1 
bltz $s0, L13:for_true_init_branch
L17:
j L12
L16:
