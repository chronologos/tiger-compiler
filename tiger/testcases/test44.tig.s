L15:
addi $FP, $SP -4
addi $SP, $SP, -60
sw $a0, 0($FP)
sw $RA, -40($FP)
sw $FP, -36($FP)
sw $s0, -32($FP)
sw $s1, -28($FP)
sw $s2, -24($FP)
sw $s3, -20($FP)
sw $s4, -16($FP)
sw $s5, -12($FP)
sw $s6, -8($FP)
sw $s7, -4($FP)
li $s0, 0
li $s0, 0
move $s2, $s0 
addi $s1, $zero,0
beq $s2, $s1, L12 
L13:
addi $s2, $zero,1
addi $s1, $zero,4
mult $s1, $s2, $s1
add $s1, $s1, $s0
lw $s1, 0($s1)
move $RV, $s1 
j L16
L12:
addi $s1, $zero,1
move $a0,$s1 
jal L14:exit
j L13
L16:
lw $RA, -40($FP)
lw $FP, -36($FP)
lw $s0, -32($FP)
lw $s1, -28($FP)
lw $s2, -24($FP)
lw $s3, -20($FP)
lw $s4, -16($FP)
lw $s5, -12($FP)
lw $s6, -8($FP)
lw $s7, -4($FP)
addi $SP, $SP, 60
jr $RA
