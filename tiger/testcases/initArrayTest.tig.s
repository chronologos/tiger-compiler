L15:
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
bgtz $a0, L14
L13:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L14:
addi $a1, $zero,5
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a1, $s2, $a0
addi $a0, $zero,29
sw $a0, 0($a1)
addi $a0, $zero,4
sub $a0, $s2, $a0
lw $a0, 0($a0)
move $a1, $a0 
addi $a0, $zero,6
sub $a0, $a1, $a0 
bgtz $a0, L12
L11:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L12:
addi $a1, $zero,6
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a1, $s2, $a0
addi $a0, $zero,36
sw $a0, 0($a1)
addi $a0, $zero,4
sub $a0, $s2, $a0
lw $a0, 0($a0)
move $a1, $a0 
addi $a0, $zero,7
sub $a0, $a1, $a0 
bgtz $a0, L6
L5:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L6:
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
bgtz $a0, L8
L7:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L8:
addi $a1, $zero,5
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a0, $s2, $a0
lw $a0, 0($a0)
move $s1, $a0 
addi $a0, $zero,4
sub $a0, $s2, $a0
lw $a0, 0($a0)
move $a1, $a0 
addi $a0, $zero,6
sub $a0, $a1, $a0 
bgtz $a0, L10
L9:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L10:
addi $a1, $zero,6
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a0, $s2, $a0
lw $a0, 0($a0)
add $a0, $s1, $a0
sw $a0, 0($s0)
addi $a0, $zero,4
sub $a0, $s2, $a0
lw $a0, 0($a0)
move $a1, $a0 
addi $a0, $zero,7
sub $a0, $a1, $a0 
bgtz $a0, L4
L3:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L4:
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
la $a0, L2
move $a0,$a0 
jal tig_print
li $v0, 0
j L16
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
L2: .word 8
.ascii "hey adi!"