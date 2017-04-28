L20:
move $fp, $sp
move $t0, $fp
addi $fp, $sp, -4
addi $sp, $sp, -64
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
addi $a0, $zero,1
move $a0,$a0 
la $a1, L18
move $a1,$a1 
addi $a2, $zero,3
move $a2,$a2 
la $a3, L19
move $a3,$a3 
addi $s0, $zero,5
sw $s0, 16($sp)
jal L12
move $v0, $v0 
j L21
L21:
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
addi $sp, $sp, 64
move $fp, $t0
jr $ra
L12:
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
move $a1, $a1
move $a0, $a2
move $a0, $a3
addi $a0, $zero,0
beq $a1, $a0, L15 
L16:
la $a0, L14
L17:
move $v0, $a0 
j L22
L15:
la $a0, L13
j L17
L22:
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
.data
L13: .ascii "nerd"
L14: .ascii "bird"
L18: .ascii "two"
L19: .ascii "four"
