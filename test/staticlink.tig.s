.text
main:
L10:   #Function start here
sw $fp, 0($sp)   #save old fp -> stack
move $fp, $sp   #move sp to fp
addiu $sp, $sp, -52   #allocate stack
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
move $a0, $fp
li $v0, 3
move $a1, $v0
jal sum
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
j L13
L13:
move $sp, $fp   #restore sp
lw $fp, 0($sp)   #restore fp
jr $ra    #jump back to return address
#Function end here

sum:   #Function start here
sw $fp, 0($sp)   #save old fp -> stack
move $fp, $sp   #move sp to fp
addiu $sp, $sp, -52   #allocate stack
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
move $a0, $fp
jal n
move $v0, $v0
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

