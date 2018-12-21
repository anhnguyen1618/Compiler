.text
main:
L16:   #Function start here
sw $fp, 0($sp)   #save old fp -> stack
move $fp, $sp   #move sp to fp
addiu $sp, $sp, -92   #allocate stack
L25:
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
li t145, 2
move $a1, t145
jal fib
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
j L24
L24:
move $sp, $fp   #restore sp
lw $fp, 0($sp)   #restore fp
jr $ra    #jump back to return address
#Function end here

fib:   #Function start here
sw $fp, 0($sp)   #save old fp -> stack
move $fp, $sp   #move sp to fp
addiu $sp, $sp, -132   #allocate stack
L27:
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
move t142, $a1
li t149, 0
beq t142,t149,L21
L22:
li t150, 1
beq t142,t150,L18
L19:
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
lw t151, -4($fp)
move $a0, t151
addi t152, t142, -1
move $a1, t152
jal fib
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
move t146, $v0
move t148, t146
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
lw t153, -4($fp)
move $a0, t153
addi t154, t142, -2
move $a1, t154
jal fib
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
move t147, $v0
add t155,t148,t147
move t143, t155
L20:
move t144, t143
L23:
move $v0, t144
lw $s0, -12($fp)
lw $s1, -16($fp)
lw $s2, -20($fp)
lw $s3, -24($fp)
lw $s4, -28($fp)
lw $s5, -32($fp)
lw $s6, -36($fp)
lw $s7, -40($fp)
lw $ra, -8($fp)
j L26
L21:
li t144, 0
j L23
L18:
li t143, 1
j L20
L26:
move $sp, $fp   #restore sp
lw $fp, 0($sp)   #restore fp
jr $ra    #jump back to return address
#Function end here

