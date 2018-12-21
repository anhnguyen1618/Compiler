.text
main:
L10:   #Function start here
sw $fp, 0($sp)   #save old fp -> stack
move $fp, $sp   #move sp to fp
addiu $sp, $sp, -92   #allocate stack
L13:
sw $s0, -12($fp)
sw $s1, -16($fp)
sw $s2, -20($fp)
sw $s3, -24($fp)
sw $s4, -28($fp)
sw $s5, -32($fp)
sw $s6, -36($fp)
sw $s7, -40($fp)
sw $ra, -8($fp)
sw $a0, -4($fp)
sw $a0, 0($fp)
sw $a1, 4($fp)
sw $a2, 8($fp)
sw $a3, 12($fp)
sw $t0, -44($fp)
sw $t1, -48($fp)
sw $t2, -52($fp)
sw $t3, -56($fp)
sw $t4, -60($fp)
sw $t5, -64($fp)
sw $t6, -68($fp)
sw $t7, -72($fp)
sw $t8, -76($fp)
sw $t9, -80($fp)
move $a0, $fp
li t133, 3
move $a1, t133
jal sum
lw $t0, -44($fp)
lw $t1, -48($fp)
lw $t2, -52($fp)
lw $t3, -56($fp)
lw $t4, -60($fp)
lw $t5, -64($fp)
lw $t6, -68($fp)
lw $t7, -72($fp)
lw $t8, -76($fp)
lw $t9, -80($fp)
lw $a0, 0($fp)
lw $a1, 4($fp)
lw $a2, 8($fp)
lw $a3, 12($fp)
move $v0, $v0
lw $s0, -12($fp)
lw $s1, -16($fp)
lw $s2, -20($fp)
lw $s3, -24($fp)
lw $s4, -28($fp)
lw $s5, -32($fp)
lw $s6, -36($fp)
lw $s7, -40($fp)
lw $ra, -8($fp)
j L12
L12:
move $sp, $fp   #restore sp
lw $fp, 0($sp)   #restore fp
jr $ra    #jump back to return address
#Function end here

sum:   #Function start here
sw $fp, 0($sp)   #save old fp -> stack
move $fp, $sp   #move sp to fp
addiu $sp, $sp, -132   #allocate stack
L15:
sw $s0, -12($fp)
sw $s1, -16($fp)
sw $s2, -20($fp)
sw $s3, -24($fp)
sw $s4, -28($fp)
sw $s5, -32($fp)
sw $s6, -36($fp)
sw $s7, -40($fp)
sw $ra, -8($fp)
sw $a0, -4($fp)
move t132, $a1
sw $a0, 0($fp)
sw $a1, 4($fp)
sw $a2, 8($fp)
sw $a3, 12($fp)
sw $t0, -44($fp)
sw $t1, -48($fp)
sw $t2, -52($fp)
sw $t3, -56($fp)
sw $t4, -60($fp)
sw $t5, -64($fp)
sw $t6, -68($fp)
sw $t7, -72($fp)
sw $t8, -76($fp)
sw $t9, -80($fp)
lw t137, -4($fp)
move $a0, t137
addi t138,t132,1
move $a1, t138
jal sum
lw $t0, -44($fp)
lw $t1, -48($fp)
lw $t2, -52($fp)
lw $t3, -56($fp)
lw $t4, -60($fp)
lw $t5, -64($fp)
lw $t6, -68($fp)
lw $t7, -72($fp)
lw $t8, -76($fp)
lw $t9, -80($fp)
lw $a0, 0($fp)
lw $a1, 4($fp)
lw $a2, 8($fp)
lw $a3, 12($fp)
move t134, $v0
move t136, t134
sw $a0, 0($fp)
sw $a1, 4($fp)
sw $a2, 8($fp)
sw $a3, 12($fp)
sw $t0, -84($fp)
sw $t1, -88($fp)
sw $t2, -92($fp)
sw $t3, -96($fp)
sw $t4, -100($fp)
sw $t5, -104($fp)
sw $t6, -108($fp)
sw $t7, -112($fp)
sw $t8, -116($fp)
sw $t9, -120($fp)
lw t139, -4($fp)
move $a0, t139
addi t140,t132,2
move $a1, t140
jal sum
lw $t0, -84($fp)
lw $t1, -88($fp)
lw $t2, -92($fp)
lw $t3, -96($fp)
lw $t4, -100($fp)
lw $t5, -104($fp)
lw $t6, -108($fp)
lw $t7, -112($fp)
lw $t8, -116($fp)
lw $t9, -120($fp)
lw $a0, 0($fp)
lw $a1, 4($fp)
lw $a2, 8($fp)
lw $a3, 12($fp)
move t135, $v0
add t141,t136,t135
move $v0, t141
lw $s0, -12($fp)
lw $s1, -16($fp)
lw $s2, -20($fp)
lw $s3, -24($fp)
lw $s4, -28($fp)
lw $s5, -32($fp)
lw $s6, -36($fp)
lw $s7, -40($fp)
lw $ra, -8($fp)
j L14
L14:
move $sp, $fp   #restore sp
lw $fp, 0($sp)   #restore fp
jr $ra    #jump back to return address
#Function end here

