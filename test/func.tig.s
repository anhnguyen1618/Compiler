L10:
sw $fp, 0($sp)
move $fp, $sp
addiu $sp, $sp, -52
L17:
sw $a0, 0($fp)
move $a0, $fp
li $v0, 0
move $a1, $v0
la $v0, L15
move $a2, $v0
jal L11
move $v0, $v0
j L16
L16:
move $sp, $fp
lw $fp, 0($sp)
jr $ra
L15 .asciiz "str2"
L12:
sw $fp, 0($sp)
move $fp, $sp
addiu $sp, $sp, -52
L19:
sw $a0, 0($fp)
move $v0, $a1
lw $v1, 0($fp)
move $a0, $v1
move $a1, $v0
la $v0, L13
move $a2, $v0
jal L11
la $v0, L14
j L18
L18:
move $sp, $fp
lw $fp, 0($sp)
jr $ra
L14 .asciiz " "
L13 .asciiz "str"
L11:
sw $fp, 0($sp)
move $fp, $sp
addiu $sp, $sp, -52
L21:
sw $a0, 0($fp)
move $v0, $a1
sw $a2, 4($fp)
lw $v1, 0($fp)
move $a0, $v1
addi $v0,$v0,1
move $a1, $v0
jal L12
li $v0, 0
j L20
L20:
move $sp, $fp
lw $fp, 0($sp)
jr $ra
