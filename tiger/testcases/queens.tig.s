L61:
move $fp, $sp
move $t0, $fp
addi $fp, $sp, -4
addi $sp, $sp, -80
sw $a0, 0($fp)
sw $ra, -60($fp)
sw $s0, -56($fp)
sw $s1, -52($fp)
sw $s2, -48($fp)
sw $s3, -44($fp)
sw $s4, -40($fp)
sw $s5, -36($fp)
sw $s6, -32($fp)
sw $s7, -28($fp)
sw $t0, -24($fp)
addi $a0, $zero,8
sw $a0, -4($fp)
addi $a0, $fp, -8
move $s0, $a0 
lw $a0, -4($fp)
addi $a0, $a0, 1
move $a0,$a0 
addi $a1, $zero,0
move $a1,$a1 
jal tig_initArray
addi $a0, $fp, -4
lw $a0, 0($a0)
sw $a0, 0($v0)
addi $a0, $v0, 4
sw $a0, 0($s0)
addi $a0, $fp, -12
move $s0, $a0 
lw $a0, -4($fp)
addi $a0, $a0, 1
move $a0,$a0 
addi $a1, $zero,0
move $a1,$a1 
jal tig_initArray
addi $a0, $fp, -4
lw $a0, 0($a0)
sw $a0, 0($v0)
addi $a0, $v0, 4
sw $a0, 0($s0)
addi $a0, $fp, -16
move $s0, $a0 
lw $a1, -4($fp)
lw $a0, -4($fp)
add $a1, $a1, $a0
addi $a0, $zero,1
sub $a0, $a1, $a0
addi $a0, $a0, 1
move $a0,$a0 
addi $a1, $zero,0
move $a1,$a1 
jal tig_initArray
lw $a1, -4($fp)
lw $a0, -4($fp)
add $a1, $a1, $a0
addi $a0, $zero,1
sub $a0, $a1, $a0
sw $a0, 0($v0)
addi $a0, $v0, 4
sw $a0, 0($s0)
addi $a0, $fp, -20
move $s0, $a0 
lw $a1, -4($fp)
lw $a0, -4($fp)
add $a1, $a1, $a0
addi $a0, $zero,1
sub $a0, $a1, $a0
addi $a0, $a0, 1
move $a0,$a0 
addi $a1, $zero,0
move $a1,$a1 
jal tig_initArray
lw $a1, -4($fp)
lw $a0, -4($fp)
add $a1, $a1, $a0
addi $a0, $zero,1
sub $a0, $a1, $a0
sw $a0, 0($v0)
addi $a0, $v0, 4
sw $a0, 0($s0)
move $a0,$t0 
addi $a1, $zero,0
move $a1,$a1 
jal L3
li $v0, 0
j L62
L62:
lw $ra, -60($fp)
lw $s0, -56($fp)
lw $s1, -52($fp)
lw $s2, -48($fp)
lw $s3, -44($fp)
lw $s4, -40($fp)
lw $s5, -36($fp)
lw $s6, -32($fp)
lw $s7, -28($fp)
lw $t0, -24($fp)
addi $sp, $sp, 80
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
move $s3, $a1
lw $a0, 0($fp)
lw $a0, -4($a0)
beq $s3, $a0, L58 
L59:
li $s2, 0
lw $a0, 0($fp)
lw $a1, -4($a0)
addi $a0, $zero,1
sub $a0, $a1, $a0
move $s0, $a0 
sub $a0, $s2, $s0 
blez $a0, L57
L19:
li $a0, 0
L60:
move $v0, $a0 
j L63
L58:
lw $a0, 0($fp)
move $a0,$a0 
jal L2
li $a0, 0
j L60
L56:
addi $a0, $s2, 1
move $s2, $a0 
L57:
lw $a0, 0($fp)
lw $a1, -8($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a0, $a0 
sub $a0, $a0, $s2 
bgtz $a0, L21
L20:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L21:
lw $a0, 0($fp)
lw $a1, -8($a0)
addi $a0, $zero,4
mult $s2, $a0
mflo $a0
add $a0, $a1, $a0
lw $a1, 0($a0)
addi $a0, $zero,0
beq $a1, $a0, L26 
L27:
li $a1, 0
L28:
addi $a0, $zero,0
beq $a0, $a1, L31 
L32:
L36:
li $a1, 0
L37:
addi $a0, $zero,0
beq $a0, $a1, L52 
L53:
L55:
sub $a0,$s2,$s0 
bltz $a0, L56
L64:
j L19
L26:
li $s1, 1
lw $a0, 0($fp)
lw $a1, -16($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a1, $a0 
add $a0, $s2, $s3
sub $a0, $a1, $a0 
bgtz $a0, L23
L22:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L23:
lw $a0, 0($fp)
lw $a2, -16($a0)
add $a1, $s2, $s3
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a0, $a2, $a0
lw $a1, 0($a0)
addi $a0, $zero,0
beq $a1, $a0, L24 
L25:
li $s1, 0
L24:
move $a1, $s1 
j L28
L31:
L35:
li $s1, 1
lw $a0, 0($fp)
lw $a1, -20($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a1, $a0 
addi $a0, $s2, 7
sub $a0, $a0, $s3
sub $a0, $a1, $a0 
bgtz $a0, L30
L29:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L30:
lw $a0, 0($fp)
lw $a2, -20($a0)
addi $a0, $s2, 7
sub $a1, $a0, $s3
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a0, $a2, $a0
lw $a1, 0($a0)
addi $a0, $zero,0
beq $a1, $a0, L33 
L34:
li $s1, 0
L33:
move $a1, $s1 
j L37
L52:
L54:
lw $a0, 0($fp)
lw $a1, -8($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a0, $a0 
sub $a0, $a0, $s2 
bgtz $a0, L51
L50:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L51:
lw $a0, 0($fp)
lw $a1, -8($a0)
addi $a0, $zero,4
mult $s2, $a0
mflo $a0
add $a1, $a1, $a0
addi $a0, $zero,1
sw $a0, 0($a1)
lw $a0, 0($fp)
lw $a1, -16($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a1, $a0 
add $a0, $s2, $s3
sub $a0, $a1, $a0 
bgtz $a0, L49
L48:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L49:
lw $a0, 0($fp)
lw $a2, -16($a0)
add $a1, $s2, $s3
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a1, $a2, $a0
addi $a0, $zero,1
sw $a0, 0($a1)
lw $a0, 0($fp)
lw $a1, -20($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a1, $a0 
addi $a0, $s2, 7
sub $a0, $a0, $s3
sub $a0, $a1, $a0 
bgtz $a0, L47
L46:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L47:
lw $a0, 0($fp)
lw $a2, -20($a0)
addi $a0, $s2, 7
sub $a1, $a0, $s3
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a1, $a2, $a0
addi $a0, $zero,1
sw $a0, 0($a1)
lw $a0, 0($fp)
lw $a1, -12($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a0, $a0 
sub $a0, $a0, $s3 
bgtz $a0, L45
L44:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L45:
lw $a0, 0($fp)
lw $a1, -12($a0)
addi $a0, $zero,4
mult $s3, $a0
mflo $a0
add $a0, $a1, $a0
sw $s2, 0($a0)
lw $a0, 0($fp)
move $a0,$a0 
addi $a1, $s3, 1
move $a1,$a1 
jal L3
lw $a0, 0($fp)
lw $a1, -8($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a0, $a0 
sub $a0, $a0, $s2 
bgtz $a0, L43
L42:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L43:
lw $a0, 0($fp)
lw $a1, -8($a0)
addi $a0, $zero,4
mult $s2, $a0
mflo $a0
add $a1, $a1, $a0
addi $a0, $zero,0
sw $a0, 0($a1)
lw $a0, 0($fp)
lw $a1, -16($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a1, $a0 
add $a0, $s2, $s3
sub $a0, $a1, $a0 
bgtz $a0, L41
L40:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L41:
lw $a0, 0($fp)
lw $a2, -16($a0)
add $a1, $s2, $s3
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a1, $a2, $a0
addi $a0, $zero,0
sw $a0, 0($a1)
lw $a0, 0($fp)
lw $a1, -20($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a1, $a0 
addi $a0, $s2, 7
sub $a0, $a0, $s3
sub $a0, $a1, $a0 
bgtz $a0, L39
L38:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L39:
lw $a0, 0($fp)
lw $a2, -20($a0)
addi $a0, $s2, 7
sub $a1, $a0, $s3
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a1, $a2, $a0
addi $a0, $zero,0
sw $a0, 0($a1)
j L55
L63:
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
li $s3, 0
lw $a0, 0($fp)
lw $a1, -4($a0)
addi $a0, $zero,1
sub $a0, $a1, $a0
move $s0, $a0 
sub $a0, $s3, $s0 
blez $a0, L18
L5:
la $a0, L4
move $a0,$a0 
jal tig_print
li $v0, 0
j L65
L17:
addi $a0, $s3, 1
move $s3, $a0 
L18:
li $s2, 0
lw $a0, 0($fp)
lw $a1, -4($a0)
addi $a0, $zero,1
sub $a0, $a1, $a0
move $s1, $a0 
sub $a0, $s2, $s1 
blez $a0, L16
L7:
la $a0, L6
move $a0,$a0 
jal tig_print
sub $a0,$s3,$s0 
bltz $a0, L17
L66:
j L5
L15:
addi $a0, $s2, 1
move $s2, $a0 
L16:
lw $a0, 0($fp)
lw $a1, -12($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a0, $a0 
sub $a0, $a0, $s3 
bgtz $a0, L9
L8:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L9:
lw $a0, 0($fp)
lw $a1, -12($a0)
addi $a0, $zero,4
mult $s3, $a0
mflo $a0
add $a0, $a1, $a0
lw $a0, 0($a0)
beq $a0, $s2, L12 
L13:
la $a0, L11
L14:
move $a0,$a0 
jal tig_print
sub $a0,$s2,$s1 
bltz $a0, L15
L67:
j L7
L12:
la $a0, L10
j L14
L65:
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
L10: .word 2
.ascii " O"
L11: .word 2
.ascii " ."
