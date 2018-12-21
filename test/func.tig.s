.text
main:
L10:   #Function start here
sw $fp, 0($sp)   #save old fp -> stack
move $fp, $sp   #move sp to fp
addiu $sp, $sp, -96   #allocate stack
L17:
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
li t134, 0
move $a1, t134
la t135, L15
move $a2, t135
jal do_nothing1
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
j L16
L16:
move $sp, $fp   #restore sp
lw $fp, 0($sp)   #restore fp
jr $ra    #jump back to return address
#Function end here

.data
L15: .asciiz "str2"
.text

do_nothing2:   #Function start here
sw $fp, 0($sp)   #save old fp -> stack
move $fp, $sp   #move sp to fp
addiu $sp, $sp, -96   #allocate stack
L19:
sw $a0, -4($fp)
move t133, $a1
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
lw t136, -4($fp)
move $a0, t136
move $a1, t133
la t137, L13
move $a2, t137
jal do_nothing1
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
la $v0, L14
lw $s0, -12($fp)
lw $s1, -16($fp)
lw $s2, -20($fp)
lw $s3, -24($fp)
lw $s4, -28($fp)
lw $s5, -32($fp)
lw $s6, -36($fp)
lw $s7, -40($fp)
lw $ra, -8($fp)
j L18
L18:
move $sp, $fp   #restore sp
lw $fp, 0($sp)   #restore fp
jr $ra    #jump back to return address
#Function end here

.data
L14: .asciiz " "
.text

.data
L13: .asciiz "str"
.text

do_nothing1:   #Function start here
sw $fp, 0($sp)   #save old fp -> stack
move $fp, $sp   #move sp to fp
addiu $sp, $sp, -96   #allocate stack
L21:
sw $a0, -4($fp)
move t132, $a1
sw $a2, -8($fp)
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
lw t138, -4($fp)
move $a0, t138
addi t139,t132,1
move $a1, t139
jal do_nothing2
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
li $v0, 0
lw $s0, -16($fp)
lw $s1, -20($fp)
lw $s2, -24($fp)
lw $s3, -28($fp)
lw $s4, -32($fp)
lw $s5, -36($fp)
lw $s6, -40($fp)
lw $s7, -44($fp)
lw $ra, -12($fp)
j L20
L20:
move $sp, $fp   #restore sp
lw $fp, 0($sp)   #restore fp
jr $ra    #jump back to return address
#Function end here

