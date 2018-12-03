EXP(
 ESEQ(
  SEQ(
   MOVE(
    TEMP t132,
    CONST 5),
   SEQ(
    MOVE(
     TEMP t133,
     CONST 6),
    MOVE(
     TEMP t134,
     CONST 7))),
  ESEQ(
   MOVE(
    TEMP t134,
    BINOP(MINUS,
     TEMP t132,
     TEMP t133)),
   CONST 0)))
MOVE(
 TEMP t132,
 CONST 5)
MOVE(
 TEMP t133,
 CONST 6)
MOVE(
 TEMP t134,
 CONST 7)
MOVE(
 TEMP t134,
 BINOP(MINUS,
  TEMP t132,
  TEMP t133))
L13:
li t132, 5
li t133, 6
li t134, 7
sub t135,t132,t133
lw t134, t135
j L12
L12:
