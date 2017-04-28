L20:
move $fp, $sp
move $t0, $fp
addi $fp, $sp, -4
addi $sp, $sp, -64
sw $a0, 0($fp)
sw $ra, -44($fp)
sw $s0, -40($fp)
sw $s1, -36($fp)
sw $s2, -32($fp)
sw $s3, -28($fp)
sw $s4, -24($fp)
sw $s5, -20($fp)
sw $s6, -16($fp)
sw $s7, -12($fp)
sw $t0, -8($fp)
addi $a0, $zero,1
sw $a0, -4($fp)
move $a0,$t0 
addi $a1, $zero,2
move $a1,$a1 
jal L12
move $v0, $v0 
j L21
L21:
lw $ra, -44($fp)
lw $s0, -40($fp)
lw $s1, -36($fp)
lw $s2, -32($fp)
lw $s3, -28($fp)
lw $s4, -24($fp)
lw $s5, -20($fp)
lw $s6, -16($fp)
lw $s7, -12($fp)
lw $t0, -8($fp)
addi $sp, $sp, 64
move $fp, $t0
jr $ra
L12:
move $t0, $fp
addi $fp, $sp, -4
addi $sp, $sp, -60
sw $a0, 0($fp)
sw $a1, 4($fp)
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
move $a0,$fp 
addi $a1, $zero,1
move $a1,$a1 
jal L13
move $v0, $v0 
j L22
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
L15:
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
move $s1, $a1
move $a0,$fp 
jal L19
move $a0, $v0 
move $s0, $a0 
lw $a0, 0($fp)
move $a0,$a0 
move $a1,$s1 
jal L14
move $a0, $v0 
add $a0, $s0, $a0
move $v0, $a0 
j L23
L23:
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
L19:
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
li $v0, 1
j L24
L24:
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
L14:
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
move $a2, $a1
addi $a0, $zero,0
beq $a2, $a0, L16 
L17:
move $s0, $a2 
lw $a0, 0($fp)
move $a0,$a0 
addi $a1, $zero,1
sub $a1, $a2, $a1
move $a1,$a1 
jal L14
move $a0, $v0 
mult $s0, $a0
mflo $a0
move $a0, $a0 
L18:
move $v0, $a0 
j L25
L16:
li $a0, 1
j L18
L25:
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
L13:
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
move $a0, $a1
lw $a0, 0($fp)
lw $a0, 0($a0)
lw $a0, -4($a0)
move $s0, $a0 
lw $a0, 0($fp)
move $a0,$a0 
lw $a1, 0($fp)
lw $a1, 4($a1)
move $a1,$a1 
jal L15
move $a0, $v0 
add $a0, $s0, $a0
addi $a0, $a0, 1
move $v0, $a0 
j L26
L26:
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
