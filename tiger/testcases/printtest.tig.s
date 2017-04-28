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
move $a0,$t0 
addi $a1, $zero,0
move $a1,$a1 
jal L3
li $v0, 0
j L14
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
L3:
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
move $a0,$a0 
jal L2
li $v0, 0
j L15
L15:
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
li $s1, 0
addi $a1, $zero,5
addi $a0, $zero,1
sub $a0, $a1, $a0
move $s0, $a0 
sub $a0, $s1, $s0 
blez $a0, L12
L5:
la $a0, L4
move $a0,$a0 
jal tig_print
li $v0, 0
j L16
L11:
addi $a0, $s1, 1
move $s1, $a0 
L12:
li $s3, 0
addi $a1, $zero,5
addi $a0, $zero,1
sub $a0, $a1, $a0
move $s2, $a0 
sub $a0, $s3, $s2 
blez $a0, L10
L7:
la $a0, L6
move $a0,$a0 
jal tig_print
sub $a0,$s1,$s0 
bltz $a0, L11
L17:
j L5
L9:
addi $a0, $s3, 1
move $s3, $a0 
L10:
la $a0, L8
move $a0,$a0 
jal tig_print
sub $a0,$s3,$s2 
bltz $a0, L9
L18:
j L7
L16:
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
L4: .word 2
.ascii "\n"
L6: .word 2
.ascii "\n"
L8: .word 2
.ascii " ."
