L17:
addi t100:FP, t101:SP -4
addi t101:SP, t101:SP, -48
sw t106:a0, 0(t100:FP)
sw t104:RA, -44(t100:FP)
sw t100:FP, -40(t100:FP)
sw t110:s0, -36(t100:FP)
sw t111:s1, -32(t100:FP)
sw t112:s2, -28(t100:FP)
sw t113:s3, -24(t100:FP)
sw t114:s4, -20(t100:FP)
sw t115:s5, -16(t100:FP)
sw t116:s6, -12(t100:FP)
sw t117:s7, -8(t100:FP)
li t128, 2
move t129, t128 
move t135, t129 
li t130, 1
sub t137, t135, t130 
bgtz t137, L14
L15:
li t132, 0
move t134, t132 
L16:
move t102:RV, t134 
j L18
L14:
li t133, 1
move t136, t129 
li t131, 1
sub t138, t136, t131 
bgtz t138, L12
L13:
li t133, 0
L12:
move t134, t133 
j L16
L18:
lw t104:RA, -44(t100:FP)
lw t100:FP, -40(t100:FP)
lw t110:s0, -36(t100:FP)
lw t111:s1, -32(t100:FP)
lw t112:s2, -28(t100:FP)
lw t113:s3, -24(t100:FP)
lw t114:s4, -20(t100:FP)
lw t115:s5, -16(t100:FP)
lw t116:s6, -12(t100:FP)
lw t117:s7, -8(t100:FP)
addi t101:SP, t101:SP, 48
jr t104:RA
L17:
addi $FP, $SP -4
addi $SP, $SP, -48
sw $a0, 0($FP)
sw $RA, -44($FP)
sw $FP, -40($FP)
sw $s0, -36($FP)
sw $s1, -32($FP)
sw $s2, -28($FP)
sw $s3, -24($FP)
sw $s4, -20($FP)
sw $s5, -16($FP)
sw $s6, -12($FP)
sw $s7, -8($FP)
li $s0, 2
move $s3, $s0 
move $s0, $s3 
li $s1, 1
sub $s0, $s0, $s1 
bgtz $s0, L14
L15:
li $s0, 0
move $s0, $s0 
L16:
move $RV, $s0 
j L18
L14:
li $s1, 1
move $s0, $s3 
li $s2, 1
sub $s0, $s0, $s2 
bgtz $s0, L12
L13:
li $s1, 0
L12:
move $s0, $s1 
j L16
L18:
lw $RA, -44($FP)
lw $FP, -40($FP)
lw $s0, -36($FP)
lw $s1, -32($FP)
lw $s2, -28($FP)
lw $s3, -24($FP)
lw $s4, -20($FP)
lw $s5, -16($FP)
lw $s6, -12($FP)
lw $s7, -8($FP)
addi $SP, $SP, 48
jr $RA
