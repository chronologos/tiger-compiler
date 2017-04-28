LABEL L18
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
 TEMP tt131,
 TEMP tt130)
LABEL L16
MOVE(
 TEMP tt135,
 TEMP tt131)
MOVE(
 TEMP tt132,
 CONST 10)
CJUMP(LT,
 TEMP tt135,
 TEMP tt132,
 L17,L12)
LABEL L12
MOVE(
 TEMP tt102:RV,
 CONST 0)
JUMP(
 NAME L19)
LABEL L17
LABEL L14
MOVE(
 TEMP tt134,
 TEMP tt129)
MOVE(
 TEMP tt133,
 CONST 10)
CJUMP(LT,
 TEMP tt134,
 TEMP tt133,
 L15,L13)
LABEL L13
JUMP(
 NAME L16)
LABEL L15
JUMP(
 NAME L13)
LABEL L19
L18:
li t128, 0
move t129, t128 
li t130, 0
move t131, t130 
L16:
move t135, t131 
li t132, 10
sub t136,t135,t132 
bltz t136, L17
L12:
li t102:RV, 0
j L19
L17:
L14:
move t134, t129 
li t133, 10
sub t137,t134,t133 
bltz t137, L15
L13:
j L16
L15:
j L13
L19:


L18:
li $s0, 0
move $s3, $s0   # s3 = i
li $s0, 0
move $s2, $s0   # s2 = j
L16:
move $s0, $s2   # s0 = j
li $s1, 10      # s1 = 10
sub $s0,$s0,$s1 # s0 = j-10
bltz $s0, L17   
L12:            # j >= 10
li $RV, 0
j L19           # jump out of outer while
L17:
L14:
move $s0, $s3 
li $s1, 10
sub $s0,$s0,$s1 
bltz $s0, L15
L13:
j L16
L15:
j L13
L19:
