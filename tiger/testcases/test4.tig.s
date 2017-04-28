L16:
move $t0, $fp
addi $fp, $sp, -4
addi $sp, $sp, -60
sw $a0, 0($fp)
sw $ra, -40($fp)
sw $s0, -36($fp)
sw $s1, -32($fp)
sw $s2, -28($fp)
sw $s3, -24($fp)
sw $s4, -20($fp)
sw $s5, -16($fp)
sw $s6, -12($fp)
sw $s7, -8($fp)
sw $t0, -4($fp)
move $a0,$t0 
addi $s0, $zero,2
move $a1,$s0 
jal L12
move $v0, $v0 
j L17
L17:
lw $ra, -40($fp)
lw $s0, -36($fp)
lw $s1, -32($fp)
lw $s2, -28($fp)
lw $s3, -24($fp)
lw $s4, -20($fp)
lw $s5, -16($fp)
lw $s6, -12($fp)
lw $s7, -8($fp)
lw $t0, -4($fp)
addi $sp, $sp, 60
move $fp, $t0
jr $ra
L12:
move $t0, $fp
addi $fp, $sp, -4
addi $sp, $sp, -60
sw $a0, -4($fp)
sw $ra, -40($fp)
sw $s0, -36($fp)
sw $s1, -32($fp)
sw $s2, -28($fp)
sw $s3, -24($fp)
sw $s4, -20($fp)
sw $s5, -16($fp)
sw $s6, -12($fp)
sw $s7, -8($fp)
sw $t0, -4($fp)
move $s2, $a1
addi $s0, $zero,0
beq $s2, $s0, L13 
L14:
move $s0, $s2 
lw $s1, 0($fp)
move $a0,$s1 
addi $s1, $zero,1
sub $s1, $s2, $s1
move $a1,$s1 
jal L12
move $s1, $v0 
mult $s0, $s1
mflo $s0
move $s0, $s0 
L15:
move $v0, $s0 
j L18
L13:
li $s0, 1
j L15
L18:
lw $ra, -40($fp)
lw $s0, -36($fp)
lw $s1, -32($fp)
lw $s2, -28($fp)
lw $s3, -24($fp)
lw $s4, -20($fp)
lw $s5, -16($fp)
lw $s6, -12($fp)
lw $s7, -8($fp)
lw $t0, -4($fp)
addi $sp, $sp, 60
move $fp, $t0
jr $ra
