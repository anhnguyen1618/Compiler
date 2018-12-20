#print int
move $t1, $v0
li $v0, 1
move $a0, $t1
syscall

#print string
move $t1, $v0
li $v0, 4
la $a0, text
syscall