.text
main:
L10:   #Function start here
sw $fp, 0($sp)   #save old fp -> stack
move $fp, $sp   #move sp to fp
addiu $sp, $sp, -92   #allocate stack
L16:
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
sw $a0, 4($fp)
sw $a1, 8($fp)
sw $a2, 12($fp)
sw $a3, 16($fp)
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
li $a1, 3
move $a1, $a1
jal fac
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
lw $a0, 4($fp)
lw $a1, 8($fp)
lw $a2, 12($fp)
lw $a3, 16($fp)
move $t1, $v0
li $v0, 1
move $a0, $t1
syscall
lw $s0, -12($fp)
lw $s1, -16($fp)
lw $s2, -20($fp)
lw $s3, -24($fp)
lw $s4, -28($fp)
lw $s5, -32($fp)
lw $s6, -36($fp)
lw $s7, -40($fp)
lw $ra, -8($fp)
j L15
L15:
#procEntryExit 2
move $sp, $fp   #restore sp
lw $fp, 0($sp)   #restore fp
jr $ra    #jump back to return address
#Function end here

fac:   #Function start here
sw $fp, 0($sp)   #save old fp -> stack
move $fp, $sp   #move sp to fp
addiu $sp, $sp, -92   #allocate stack
L18:
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
move $a1, $a1
li $s0, 1
beq $a1,$s0,L12
L13:
move $s0, $a1
sw $a0, 4($fp)
sw $a1, 8($fp)
sw $a2, 12($fp)
sw $a3, 16($fp)
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
lw $a0, -4($fp)
move $a0, $a0
addi $a1, $a1, -1
move $a1, $a1
jal fac
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
lw $a0, 4($fp)
lw $a1, 8($fp)
lw $a2, 12($fp)
lw $a3, 16($fp)
move $a0, $v0
mul $a0,$s0,$a0
move $a0, $a0
L14:
move $v0, $a0
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
L12:
li $a0, 1
j L14
L17:
#procEntryExit 2
move $sp, $fp   #restore sp
lw $fp, 0($sp)   #restore fp
jr $ra    #jump back to return address
#Function end here

