L15:
addi $fp, $sp, -4
addi $sp, $sp, -60
sw $a0, 0($fp)
sw $ra, -40($fp)
sw $fp, -36($fp)
sw $s0, -32($fp)
sw $s1, -28($fp)
sw $s2, -24($fp)
sw $s3, -20($fp)
sw $s4, -16($fp)
sw $s5, -12($fp)
sw $s6, -8($fp)
sw $s7, -4($fp)
lw $s0, 0($fp)
move $a0,$s0 
addi $s0, $zero,0
move $a1,$s0 
la $s0, L14
move $a2,$s0 
jal L12
move $v0, $v0 
j L16
L16:
lw $ra, -40($fp)
lw $fp, -36($fp)
lw $s0, -32($fp)
lw $s1, -28($fp)
lw $s2, -24($fp)
lw $s3, -20($fp)
lw $s4, -16($fp)
lw $s5, -12($fp)
lw $s6, -8($fp)
lw $s7, -4($fp)
addi $sp, $sp, 60
jr $ra
L14: .ascii "str2"
L12:
addi $fp, $sp, -4
addi $sp, $sp, -60
sw $a0, -8($fp)
sw $ra, -40($fp)
sw $fp, -36($fp)
sw $s0, -32($fp)
sw $s1, -28($fp)
sw $s2, -24($fp)
sw $s3, -20($fp)
sw $s4, -16($fp)
sw $s5, -12($fp)
sw $s6, -8($fp)
sw $s7, -4($fp)
move $s1, $a1
move $s0, $a2
lw $s0, 0($fp)
move $a0,$s0 
addi $s0, $s1, 1
move $a1,$s0 
jal L13
li $v0, 0
j L17
L17:
lw $ra, -40($fp)
lw $fp, -36($fp)
lw $s0, -32($fp)
lw $s1, -28($fp)
lw $s2, -24($fp)
lw $s3, -20($fp)
lw $s4, -16($fp)
lw $s5, -12($fp)
lw $s6, -8($fp)
lw $s7, -4($fp)
addi $sp, $sp, 60
jr $ra
