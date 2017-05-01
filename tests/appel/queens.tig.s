L16:
li t128, 8
move t106:a0,t118:t0 
jal L2
li t102:v0, 0
j L17
L17:
L16:
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
li $a0, 8
move $a0,$t0 
jal L2
li $v0, 0
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
L2:
li t134, 0
li t135, 8
sub t136, t134, t135 
blez t136, L15
L4:
la t137, L3
move t106:a0,t137 
jal tig_print
li t102:v0, 0
j L18
L14:
addi t138, t134, 1
move t134, t138 
L15:
li t132, 0
li t133, 8
sub t139, t132, t133 
blez t139, L13
L6:
la t140, L5
move t106:a0,t140 
jal tig_print
sub t141,t134,t135 
bltz t141, L14
L19:
j L4
L12:
addi t142, t132, 1
move t132, t142 
L13:
beq t129, t130, L9 
L10:
la t131, L8
L11:
move t106:a0,t131 
jal tig_print
sub t144,t132,t133 
bltz t144, L12
L20:
j L6
L9:
la t131, L7
j L11
L18:
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
li $s0, 8
sub $a0, $s1, $s0 
blez $a0, L15
L4:
la $a0, L3
move $a0,$a0 
jal tig_print
li $v0, 0
j L18
L14:
addi $a0, $s1, 1
move $s1, $a0 
L15:
li $s3, 0
li $s2, 8
sub $a0, $s3, $s2 
blez $a0, L13
L6:
la $a0, L5
move $a0,$a0 
jal tig_print
sub $a0,$s1,$s0 
bltz $a0, L14
L19:
j L4
L12:
addi $a0, $s3, 1
move $s3, $a0 
L13:
beq $s4, $s4, L9 
L10:
la $a0, L8
L11:
move $a0,$a0 
jal tig_print
sub $a0,$s3,$s2 
bltz $a0, L12
L20:
j L6
L9:
la $a0, L7
j L11
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
.data
L3: .word 2
.ascii "\n"
L5: .word 2
.ascii "\n"
L7: .word 2
.ascii " O"
L8: .word 2
.ascii " ."
