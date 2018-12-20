.text
main:
L10:   #Function start here
sw $fp, 0($sp)   #save old fp -> stack
move $fp, $sp   #move sp to fp
addiu $sp, $sp, -92   #allocate stack
L14:
sw $a0, -4($fp)
sw $ra, -8($fp)
sw $s0, -12($fp)
sw $s1, -16($fp)
sw $s2, -20($fp)
sw $s3, -24($fp)
sw $s4, -28($fp)
sw $s5, -32($fp)
sw $s6, -36($fp)
sw $s7, -40($fp)
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
li $v0, 3
move $a1, $v0
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
j L13
L13:
move $sp, $fp   #restore sp
lw $fp, 0($sp)   #restore fp
jr $ra    #jump back to return address
#Function end here

sum:   #Function start here
sw $fp, 0($sp)   #save old fp -> stack
move $fp, $sp   #move sp to fp
addiu $sp, $sp, -92   #allocate stack
L16:
sw $a0, -4($fp)
sw $a1, -8($fp)
sw $ra, -12($fp)
sw $s0, -16($fp)
sw $s1, -20($fp)
sw $s2, -24($fp)
sw $s3, -28($fp)
sw $s4, -32($fp)
sw $s5, -36($fp)
sw $s6, -40($fp)
sw $s7, -44($fp)
sw $a0, 0($fp)
sw $a1, 4($fp)
sw $a2, 8($fp)
sw $a3, 12($fp)
sw $t0, -48($fp)
sw $t1, -52($fp)
sw $t2, -56($fp)
sw $t3, -60($fp)
sw $t4, -64($fp)
sw $t5, -68($fp)
sw $t6, -72($fp)
sw $t7, -76($fp)
sw $t8, -80($fp)
sw $t9, -84($fp)
move $a0, $fp
jal n
lw $t0, -48($fp)
lw $t1, -52($fp)
lw $t2, -56($fp)
lw $t3, -60($fp)
lw $t4, -64($fp)
lw $t5, -68($fp)
lw $t6, -72($fp)
lw $t7, -76($fp)
lw $t8, -80($fp)
lw $t9, -84($fp)
lw $a0, 0($fp)
lw $a1, 4($fp)
lw $a2, 8($fp)
lw $a3, 12($fp)
move $t1, $v0
li $v0, 1
move $a0, $t1
syscall
lw $s0, -16($fp)
lw $s1, -20($fp)
lw $s2, -24($fp)
lw $s3, -28($fp)
lw $s4, -32($fp)
lw $s5, -36($fp)
lw $s6, -40($fp)
lw $s7, -44($fp)
lw $ra, -12($fp)
j L15
L15:
move $sp, $fp   #restore sp
lw $fp, 0($sp)   #restore fp
jr $ra    #jump back to return address
#Function end here

n:   #Function start here
sw $fp, 0($sp)   #save old fp -> stack
move $fp, $sp   #move sp to fp
addiu $sp, $sp, -44   #allocate stack
L18:
sw $a0, -4($fp)
sw $ra, -8($fp)
sw $s0, -12($fp)
sw $s1, -16($fp)
sw $s2, -20($fp)
sw $s3, -24($fp)
sw $s4, -28($fp)
sw $s5, -32($fp)
sw $s6, -36($fp)
sw $s7, -40($fp)
lw $v0, -4($fp)
lw $v0, -8($v0)
addi $v0,$v0,5
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
j L17
L17:
move $sp, $fp   #restore sp
lw $fp, 0($sp)   #restore fp
jr $ra    #jump back to return address
#Function end here

