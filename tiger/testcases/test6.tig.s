L16:
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
lw $s0, 0($FP)
move $a0,$s0 
addi $s0, $zero,0
move $a1,$s0 
la $s0, L15
move $a2,$s0 
jal L12
li $RV, 0
j L17
L17:
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
L15: .ascii "str2"
L13:
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
move $s0, $a1
lw $s0, 0($FP)
move $a0,$s0 
lw $s0, 0($FP)
move $a1,$s0 
la $s0, L14
move $a2,$s0 
jal L12
li $RV, 0
j L18
L18:
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
L14: .ascii "str"
L12:
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
move $s0, $a2
move $s1, $a1
li $s0, 1
lw $s0, 0($FP)
move $a0,$s0 
addi $s0, $s1, 1
move $a1,$s0 
jal L13
li $RV, 0
j L19
L19:
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
