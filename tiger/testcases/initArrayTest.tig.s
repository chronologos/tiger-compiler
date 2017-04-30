L33:
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
li $s0, 8
addi $a0, $s0, 1
move $a0,$a0 
addi $a1, $zero,0
move $a1,$a1 
jal tig_initArray
sw $s0, 0($v0)
addi $a0, $v0, 4
move $s2, $a0 
addi $a0, $zero,4
sub $a0, $s2, $a0
lw $a0, 0($a0)
move $a1, $a0 
addi $a0, $zero,5
sub $a0, $a1, $a0 
bgtz $a0, L32
L31:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L32:
addi $a1, $zero,5
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
addi $a0, $zero,6
sub $a0, $a1, $a0 
bgtz $a0, L30
L29:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L30:
addi $a1, $zero,6
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
addi $a0, $zero,7
sub $a0, $a1, $a0 
bgtz $a0, L5
L4:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L5:
addi $a1, $zero,7
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a0, $s2, $a0
move $s0, $a0 
addi $a0, $zero,4
sub $a0, $s2, $a0
lw $a0, 0($a0)
move $a1, $a0 
addi $a0, $zero,5
sub $a0, $a1, $a0 
bgtz $a0, L7
L6:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L7:
addi $a1, $zero,5
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a0, $s2, $a0
lw $a1, 0($a0)
addi $a0, $zero,1
beq $a1, $a0, L12 
L13:
li $a1, 0
L14:
addi $a0, $zero,0
beq $a0, $a1, L17 
L18:
L22:
li $a1, 0
L23:
addi $a0, $zero,0
beq $a0, $a1, L24 
L25:
L27:
li $a0, 66
L28:
sw $a0, 0($s0)
addi $a0, $zero,4
sub $a0, $s2, $a0
lw $a0, 0($a0)
move $a1, $a0 
addi $a0, $zero,7
sub $a0, $a1, $a0 
bgtz $a0, L3
L2:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L3:
addi $a1, $zero,7
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a0, $s2, $a0
lw $a0, 0($a0)
move $a0,$a0 
jal tig_chr
move $a0, $v0 
move $a0,$a0 
jal tig_print
jal tig_flush
li $v0, 0
j L34
L12:
li $s1, 1
addi $a0, $zero,4
sub $a0, $s2, $a0
lw $a0, 0($a0)
move $a1, $a0 
addi $a0, $zero,6
sub $a0, $a1, $a0 
bgtz $a0, L9
L8:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L9:
addi $a1, $zero,6
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a0, $s2, $a0
lw $a1, 0($a0)
addi $a0, $zero,1
beq $a1, $a0, L10 
L11:
li $s1, 0
L10:
move $a1, $s1 
j L14
L17:
L21:
li $s1, 1
addi $a0, $zero,4
sub $a0, $s2, $a0
lw $a0, 0($a0)
move $a1, $a0 
addi $a0, $zero,1
sub $a0, $a1, $a0 
bgtz $a0, L16
L15:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L16:
addi $a1, $zero,1
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a0, $s2, $a0
lw $a1, 0($a0)
addi $a0, $zero,0
beq $a1, $a0, L19 
L20:
li $s1, 0
L19:
move $a1, $s1 
j L23
L24:
L26:
li $a0, 65
j L28
L34:
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
