L62:
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
addi $a0, $zero,4
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
j L63
L63:
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
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, -4($a0)
beq $s3, $a0, L59 
L60:
li $s2, 0
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -4($a0)
addi $a0, $zero,1
sub $a0, $a1, $a0
move $s0, $a0 
sub $a0, $s2, $s0 
blez $a0, L58
L20:
li $a0, 0
L61:
move $v0, $a0 
j L64
L59:
lw $a0, 0($fp)
move $a0,$a0 
jal L2
li $a0, 0
j L61
L57:
addi $a0, $s2, 1
move $s2, $a0 
L58:
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -8($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a0, $a0 
sub $a0, $a0, $s2 
bgtz $a0, L22
L21:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L22:
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -8($a0)
addi $a0, $zero,4
mult $s2, $a0
mflo $a0
add $a0, $a1, $a0
lw $a1, 0($a0)
addi $a0, $zero,0
beq $a1, $a0, L27 
L28:
li $a1, 0
L29:
addi $a0, $zero,0
beq $a0, $a1, L32 
L33:
L37:
li $a1, 0
L38:
addi $a0, $zero,0
beq $a0, $a1, L53 
L54:
L56:
sub $a0,$s2,$s0 
bltz $a0, L57
L65:
j L20
L27:
li $s1, 1
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -16($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a1, $a0 
add $a0, $s2, $s3
sub $a0, $a1, $a0 
bgtz $a0, L24
L23:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L24:
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a2, -16($a0)
add $a1, $s2, $s3
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a0, $a2, $a0
lw $a1, 0($a0)
addi $a0, $zero,0
beq $a1, $a0, L25 
L26:
li $s1, 0
L25:
move $a1, $s1 
j L29
L32:
L36:
li $s1, 1
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -20($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a2, $a0 
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, -4($a0)
add $a1, $s2, $a0
addi $a0, $zero,1
sub $a0, $a1, $a0
sub $a0, $a0, $s3
sub $a0, $a2, $a0 
bgtz $a0, L31
L30:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L31:
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a2, -20($a0)
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, -4($a0)
add $a1, $s2, $a0
addi $a0, $zero,1
sub $a0, $a1, $a0
sub $a1, $a0, $s3
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a0, $a2, $a0
lw $a1, 0($a0)
addi $a0, $zero,0
beq $a1, $a0, L34 
L35:
li $s1, 0
L34:
move $a1, $s1 
j L38
L53:
L55:
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -8($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a0, $a0 
sub $a0, $a0, $s2 
bgtz $a0, L52
L51:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L52:
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -8($a0)
addi $a0, $zero,4
mult $s2, $a0
mflo $a0
add $a1, $a1, $a0
addi $a0, $zero,1
sw $a0, 0($a1)
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -16($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a1, $a0 
add $a0, $s2, $s3
sub $a0, $a1, $a0 
bgtz $a0, L50
L49:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L50:
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a2, -16($a0)
add $a1, $s2, $s3
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a1, $a2, $a0
addi $a0, $zero,1
sw $a0, 0($a1)
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -20($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a2, $a0 
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, -4($a0)
add $a1, $s2, $a0
addi $a0, $zero,1
sub $a0, $a1, $a0
sub $a0, $a0, $s3
sub $a0, $a2, $a0 
bgtz $a0, L48
L47:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L48:
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a2, -20($a0)
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, -4($a0)
add $a1, $s2, $a0
addi $a0, $zero,1
sub $a0, $a1, $a0
sub $a1, $a0, $s3
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a1, $a2, $a0
addi $a0, $zero,1
sw $a0, 0($a1)
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -12($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a0, $a0 
sub $a0, $a0, $s3 
bgtz $a0, L46
L45:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L46:
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
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
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -8($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a0, $a0 
sub $a0, $a0, $s2 
bgtz $a0, L44
L43:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L44:
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -8($a0)
addi $a0, $zero,4
mult $s2, $a0
mflo $a0
add $a1, $a1, $a0
addi $a0, $zero,0
sw $a0, 0($a1)
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -16($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a1, $a0 
add $a0, $s2, $s3
sub $a0, $a1, $a0 
bgtz $a0, L42
L41:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L42:
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a2, -16($a0)
add $a1, $s2, $s3
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a1, $a2, $a0
addi $a0, $zero,0
sw $a0, 0($a1)
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -20($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a2, $a0 
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, -4($a0)
add $a1, $s2, $a0
addi $a0, $zero,1
sub $a0, $a1, $a0
sub $a0, $a0, $s3
sub $a0, $a2, $a0 
bgtz $a0, L40
L39:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L40:
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a2, -20($a0)
lw $a1, 0($fp)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, -4($a0)
add $a1, $s2, $a0
addi $a0, $zero,1
sub $a0, $a1, $a0
sub $a1, $a0, $s3
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a1, $a2, $a0
addi $a0, $zero,0
sw $a0, 0($a1)
j L56
L64:
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
move $a0,$fp 
jal L4
li $v0, 0
j L66
L66:
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
L4:
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
lw $a1, 0($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -4($a0)
addi $a0, $zero,1
sub $a0, $a1, $a0
move $s0, $a0 
sub $a0, $s3, $s0 
blez $a0, L19
L6:
la $a0, L5
move $a0,$a0 
jal tig_print
jal tig_flush
li $v0, 0
j L67
L18:
addi $a0, $s3, 1
move $s3, $a0 
L19:
li $s2, 0
lw $a0, 0($fp)
lw $a1, 0($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -4($a0)
addi $a0, $zero,1
sub $a0, $a1, $a0
move $s1, $a0 
sub $a0, $s2, $s1 
blez $a0, L17
L8:
la $a0, L7
move $a0,$a0 
jal tig_print
sub $a0,$s3,$s0 
bltz $a0, L18
L68:
j L6
L16:
addi $a0, $s2, 1
move $s2, $a0 
L17:
lw $a0, 0($fp)
lw $a1, 0($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -12($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a0, 0($a0)
move $a0, $a0 
sub $a0, $a0, $s3 
bgtz $a0, L10
L9:
addi $a0, $zero,1
move $a0,$a0 
jal exit
L10:
lw $a0, 0($fp)
lw $a1, 0($a0)
addi $a0, $zero,4
sub $a0, $a1, $a0
lw $a1, -12($a0)
addi $a0, $zero,4
mult $s3, $a0
mflo $a0
add $a0, $a1, $a0
lw $a0, 0($a0)
beq $a0, $s2, L13 
L14:
la $a0, L12
L15:
move $a0,$a0 
jal tig_print
sub $a0,$s2,$s1 
bltz $a0, L16
L69:
j L8
L13:
la $a0, L11
j L15
L67:
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
L5: .word 2
.ascii "\n"
L7: .word 2
.ascii "\n"
L11: .word 2
.ascii " O"
L12: .word 2
.ascii " ."
