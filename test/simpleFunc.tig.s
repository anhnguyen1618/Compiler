L10:
sw $fp, 0($sp)
move $fp, $sp
addiu $sp, $sp, 48
L13:
sw $a0, 0($fp)
move $a0, $fp
li $v0, 3
move $a1, $v0
jal L11
move $v0, $v0
j L12
L12:
move $sp, $fp
lw $fp, 0($sp)
jr $ra
L11:
sw $fp, 0($sp)
move $fp, $sp
addiu $sp, $sp, 40
L15:
sw $a0, 0($fp)
move $v0, $a1
addi $v0,$v0,1
move $v0, $v0
j L14
L14:
move $sp, $fp
lw $fp, 0($sp)
jr $ra
