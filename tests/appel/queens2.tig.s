L6:
li t128:localVarTemp, 8
move t106:a0,t118:t0 
addi t133:T.CONST i, $zero,0
move t107:a1,t133:T.CONST i 
jal L2
li t102:v0, 0
j L7
L7:
L2:
li t131:for_lo, 0
li t132:for_hi, 5
sub t135:comp_diff, t131:for_lo, t132:for_hi 
blez t135:comp_diff, L5
L3:
li t102:v0, 0
j L8
L4:
addi t136:T.BINOP(T.PLUS, e1, T.CONST i), t131:for_lo, 1
move t131:for_lo, t136:T.BINOP(T.PLUS, e1, T.CONST i) 
L5:
move t106:a0,t130:localVarTemp 
jal tig_chr
move t134, t102:v0 
move t106:a0,t134 
jal tig_print
sub t137:comp_diff,t131:for_lo,t132:for_hi 
bltz t137:comp_diff, L4
L9:
j L3
L8:
.data
