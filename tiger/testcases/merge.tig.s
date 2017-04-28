L95:
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
addi $a0, $fp, -4
move $s0, $a0 
jal tig_getchar
move $a0, $v0 
sw $a0, 0($s0)
move $a0,$t0 
jal L32
move $a0, $v0 
addi $a0, $fp, -4
move $s0, $a0 
jal tig_getchar
move $a0, $v0 
sw $a0, 0($s0)
move $a0,$t0 
jal L32
move $a0, $v0 
move $s0, $t0 
addi $a0, $zero,1
move $a0,$a0 
jal exit
move $a1, $v0 
move $a0,$s0 
move $a1,$a1 
jal L35
li $v0, 0
j L96
L96:
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
L35:
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
move $s2, $a1
addi $a0, $zero,0
beq $s2, $a0, L81 
L82:
li $a1, 0
L83:
addi $a0, $zero,0
beq $a0, $a1, L90 
L91:
L93:
lw $a0, 0($fp)
move $s1, $a0 
move $a1, $s2 
addi $a0, $zero,0
beq $a1, $a0, L88 
L89:
move $a0,$s1 
addi $a2, $zero,0
addi $a1, $zero,4
mult $a2, $a1
mflo $a1
add $a1, $a1, $s2
lw $a1, 0($a1)
move $a1,$a1 
jal L34
la $a0, L87
move $a0,$a0 
jal tig_print
lw $a0, 0($fp)
move $s0, $a0 
move $a1, $s2 
addi $a0, $zero,0
beq $a1, $a0, L85 
L86:
move $a0,$s0 
addi $a2, $zero,1
addi $a1, $zero,4
mult $a2, $a1
mflo $a1
add $a1, $a1, $s2
lw $a1, 0($a1)
move $a1,$a1 
jal L35
li $a0, 0
L94:
move $v0, $a0 
j L97
L81:
li $a1, 1
j L83
L90:
L92:
la $a0, L84
move $a0,$a0 
jal tig_print
li $a0, 0
j L94
L88:
addi $a0, $zero,1
move $a0,$a0 
jal exit
j L89
L85:
addi $a0, $zero,1
move $a0,$a0 
jal exit
j L86
L97:
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
L34:
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
move $s0, $a1
addi $a0, $zero,0
sub $a0,$s0,$a0 
bltz $a0, L78
L79:
addi $a0, $zero,0
sub $a0, $s0, $a0 
bgtz $a0, L75
L76:
la $a0, L74
move $a0,$a0 
jal tig_print
li $a0, 0
L77:
move $a0, $a0 
L80:
move $v0, $a0 
j L98
L78:
la $a0, L73
move $a0,$a0 
jal tig_print
move $a0,$fp 
addi $a1, $zero,0
sub $a1, $a1, $s0
move $a1,$a1 
jal L69
li $a0, 0
j L80
L75:
move $a0,$fp 
move $a1,$s0 
jal L69
li $a0, 0
j L77
L98:
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
L69:
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
move $s0, $a1
addi $a0, $zero,0
sub $a0, $s0, $a0 
bgtz $a0, L71
L72:
li $v0, 0
j L99
L71:
lw $a0, 0($fp)
move $a0,$a0 
addi $a1, $zero,10
div $s0, $a1
mflo $a1
move $a1,$a1 
jal L69
addi $a0, $zero,10
div $s0, $a0
mflo $a1
addi $a0, $zero,10
mult $a1, $a0
mflo $a0
sub $a0, $s0, $a0
move $s1, $a0 
la $a0, L70
move $a0,$a0 
jal tig_ord
move $a0, $v0 
add $a0, $s1, $a0
move $a0,$a0 
jal tig_chr
move $a0, $v0 
move $a0,$a0 
jal tig_print
j L72
L99:
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
L33:
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
move $, $a1
move $, $a2
addi $, $zero,0
beq $, $, L38 
L39:
li $, 0
L40:
addi $, $zero,0
beq $, $, L64 
L65:
L67:
addi $, $zero,0
beq $, $, L41 
L42:
li $, 0
L43:
addi $, $zero,0
beq $, $, L59 
L60:
L62:
move $, $ 
addi $, $zero,0
beq $, $, L44 
L45:
addi $, $zero,0
addi $, $zero,4
mult $, $
mflo $
add $, $, $
lw $, 0($)
move $, $ 
move $, $ 
addi $, $zero,0
beq $, $, L46 
L47:
addi $, $zero,0
addi $, $zero,4
mult $, $
mflo $
add $, $, $
lw $, 0($)
sub $,$,$ 
bltz $, L56
L57:
addi $, $zero,8
move $a0,$ 
jal malloc
move $, $v0 
addi $, $zero,0
addi $, $zero,4
mult $, $
mflo $
add $, $, $
move $s3, $ 
move $, $ 
addi $, $zero,0
beq $, $, L52 
L53:
addi $, $zero,0
addi $, $zero,4
mult $, $
mflo $
add $, $, $
lw $, 0($)
sw $, 0($s3)
addi $, $zero,1
addi $, $zero,4
mult $, $
mflo $
add $, $, $
move $s0, $ 
lw $, 0($fp)
move $s1, $ 
move $s2, $ 
move $, $ 
addi $, $zero,0
beq $, $, L54 
L55:
move $a0,$s1 
move $a1,$s2 
addi $, $zero,1
addi $, $zero,4
mult $, $
mflo $
add $, $, $
lw $, 0($)
move $a2,$ 
jal L33
move $, $v0 
sw $, 0($s0)
move $, $ 
L58:
move $, $ 
L63:
move $, $ 
L68:
move $v0, $ 
j L100
L38:
li $, 1
j L40
L64:
L66:
move $, $ 
j L68
L41:
li $, 1
j L43
L59:
L61:
move $, $ 
j L63
L44:
addi $, $zero,1
move $a0,$ 
jal exit
j L45
L46:
addi $, $zero,1
move $a0,$ 
jal exit
j L47
L56:
addi $, $zero,8
move $a0,$ 
jal malloc
move $, $v0 
addi $, $zero,0
addi $, $zero,4
mult $, $
mflo $
add $, $, $
move $, $ 
move $, $ 
addi $, $zero,0
beq $, $, L48 
L49:
addi $, $zero,0
addi $, $zero,4
mult $, $
mflo $
add $, $, $
lw $, 0($)
sw $, 0($)
addi $, $zero,1
addi $, $zero,4
mult $, $
mflo $
add $, $, $
move $s4, $ 
lw $, 0($fp)
move $, $ 
move $, $ 
addi $, $zero,0
beq $, $, L50 
L51:
move $a0,$ 
addi $, $zero,1
addi $, $zero,4
mult $, $
mflo $
add $, $, $
lw $, 0($)
move $a1,$ 
move $a2,$ 
jal L33
move $, $v0 
sw $, 0($s4)
move $, $ 
j L58
L48:
addi $, $zero,1
move $a0,$ 
jal exit
j L49
L50:
addi $, $zero,1
move $a0,$ 
jal exit
j L51
L52:
addi $, $zero,1
move $a0,$ 
jal exit
j L53
L54:
addi $, $zero,1
move $a0,$ 
jal exit
j L55
L100:
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
L32:
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
addi $a0, $zero,4
move $a0,$a0 
jal malloc
move $a2, $v0 
addi $a1, $zero,0
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a1, $a2, $a0
addi $a0, $zero,0
sw $a0, 0($a1)
move $a1, $a2 
lw $a0, 0($fp)
move $a0,$a0 
move $a1,$a1 
jal L2
move $s1, $v0 
addi $a0, $zero,8
move $a0,$a0 
jal malloc
move $s0, $v0 
addi $a1, $zero,0
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a0, $s0, $a0
sw $s1, 0($a0)
addi $a1, $zero,1
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a0, $s0, $a0
move $s1, $a0 
lw $a0, 0($fp)
move $a0,$a0 
jal L32
move $a0, $v0 
sw $a0, 0($s1)
move $v0, $s0 
j L101
L101:
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
move $s2, $a1
li $s1, 0
move $a0,$fp 
jal L4
move $a1, $s2 
addi $a0, $zero,0
beq $a1, $a0, L30 
L31:
addi $a1, $zero,0
addi $a0, $zero,4
mult $a1, $a0
mflo $a0
add $a0, $a0, $s2
move $s0, $a0 
move $a0,$fp 
lw $a1, 0($fp)
lw $a1, -4($a1)
move $a1,$a1 
jal L3
move $a0, $v0 
sw $a0, 0($s0)
L26:
move $a0,$fp 
lw $a1, 0($fp)
lw $a1, -4($a1)
move $a1,$a1 
jal L3
move $a1, $v0 
addi $a0, $zero,0
beq $a0, $a1, L28 
L29:
L24:
move $v0, $s1 
j L102
L30:
addi $a0, $zero,1
move $a0,$a0 
jal exit
j L31
L27:
addi $a0, $zero,10
mult $s1, $a0
mflo $a0
move $s0, $a0 
lw $a0, 0($fp)
lw $a0, -4($a0)
move $a0,$a0 
jal tig_ord
move $a0, $v0 
add $a0, $s0, $a0
move $s0, $a0 
la $a0, L25
move $a0,$a0 
jal tig_ord
move $a0, $v0 
sub $a0, $s0, $a0
move $s1, $a0 
lw $a0, 0($fp)
addi $a0, $a0, -4
move $s0, $a0 
jal tig_getchar
move $a0, $v0 
sw $a0, 0($s0)
j L26
L28:
j L27
L102:
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
L20:
lw $a0, 0($fp)
lw $a0, 0($a0)
lw $a0, -4($a0)
move $a0,$a0 
la $a1, L13
move $a1,$a1 
jal tig_stringEqual
addi $a0, $zero,1
beq $v0, $a0, L17 
L18:
li $s0, 1
lw $a0, 0($fp)
lw $a0, 0($a0)
lw $a0, -4($a0)
move $a0,$a0 
la $a1, L14
move $a1,$a1 
jal tig_stringEqual
addi $a0, $zero,1
beq $v0, $a0, L15 
L16:
li $s0, 0
L15:
move $a1, $s0 
L19:
addi $a0, $zero,0
beq $a0, $a1, L22 
L23:
L12:
li $v0, 0
j L103
L21:
lw $a0, 0($fp)
lw $a0, 0($a0)
addi $a0, $a0, -4
move $s0, $a0 
jal tig_getchar
move $a0, $v0 
sw $a0, 0($s0)
j L20
L17:
li $a1, 1
j L19
L22:
j L21
L103:
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
lw $a0, 0($a0)
lw $a0, -4($a0)
move $a0,$a0 
jal tig_ord
move $a0, $v0 
move $s0, $a0 
la $a0, L5
move $a0,$a0 
jal tig_ord
move $a0, $v0 
sub $a0, $s0, $a0 
bgez $a0, L9
L10:
li $a0, 0
L11:
move $v0, $a0 
j L104
L9:
li $s0, 1
lw $a0, 0($fp)
lw $a0, 0($a0)
lw $a0, -4($a0)
move $a0,$a0 
jal tig_ord
move $a0, $v0 
move $s1, $a0 
la $a0, L6
move $a0,$a0 
jal tig_ord
move $a0, $v0 
sub $a0, $s1, $a0 
blez $a0, L7
L8:
li $s0, 0
L7:
move $a0, $s0 
j L11
L104:
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
L5: .word 1
.ascii "0"
L6: .word 1
.ascii "9"
L13: .word 1
.ascii " "
L14: .word 2
.ascii "\n"
L25: .word 1
.ascii "0"
L70: .word 1
.ascii "0"
L73: .word 1
.ascii "-"
L74: .word 1
.ascii "0"
L84: .word 2
.ascii "\n"
L87: .word 1
.ascii " "
