L13:
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
addi $a0, $zero,5
addi $a0, $a0, 1
move $a0,$a0 
addi $a1, $zero,0
move $a1,$a1 
jal tig_initArray
addi $a0, $zero,5
sw $a0, 0($v0)
addi $a0, $v0, 4
move $s2, $a0 
li $s1, 1
li $s0, 1
addi $a0, $zero,4
sub $a0, $s2, $a0
lw $a0, 0($a0)
move $a1, $a0 
addi $a0, $zero,1
sub $a0, $a1, $a0 
bgtz $a0, L12
L11:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L12:
addi $a1, $zero,1
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a1, $s2, $a0
addi $a0, $zero,1
sw $a0, 0($a1)
addi $a0, $zero,4
sub $a0, $s2, $a0
lw $a0, 0($a0)
move $a1, $a0 
addi $a0, $zero,2
sub $a0, $a1, $a0 
bgtz $a0, L10
L9:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L10:
addi $a1, $zero,2
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a1, $s2, $a0
addi $a0, $zero,1
sw $a0, 0($a1)
addi $a0, $zero,4
sub $a0, $s2, $a0
lw $a0, 0($a0)
move $a0, $a0 
sub $a0, $a0, $s1 
bgtz $a0, L3
L2:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L3:
addi $a0, $zero,4
mult $s1, $a0
mflo $a0
add $a0, $s2, $a0
lw $a0, 0($a0)
beq $a0, $s0, L6 
L7:
la $a0, L5
L8:
move $a0,$a0 
jal tig_print
li $v0, 0
j L14
L6:
la $a0, L4
j L8
L14:
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
L4: .word 1
.ascii "3"
L5: .word 1
.ascii "4"
