L6:
move $fp, $sp
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
move $a0,$t0            # t0 = outermost(lev 0) fp
addi $a1, $zero,3       # a1=3
move $a1,$a1 
jal L2
move $v0, $v0 
j L7
L7:
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
L2:
move $t0, $fp           # t0 = lev 0 fp
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
beq $a2, $a0, L3 
L4:
move $s0, $a2 
lw $a0, 0($fp)
move $a0,$a0 
addi $a1, $zero,1
sub $a1, $a2, $a1
move $a1,$a1 
jal L2
move $a0, $v0 
mult $s0, $a0
mflo $a0
move $a0, $a0 
L5:
move $v0, $a0 
j L8
L3:
li $a0, 1
j L5
L8:
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
