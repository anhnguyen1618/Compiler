EXP(
 ESEQ(
  SEQ(
   MOVE(
    TEMP t144,
    CONST 5),
   SEQ(
    MOVE(
     TEMP t145,
     CONST 6),
    MOVE(
     TEMP t146,
     CONST 7))),
  ESEQ(
   MOVE(
    TEMP t146,
    BINOP(MINUS,
     TEMP t144,
     TEMP t145)),
   CONST 0)))
MOVE(
 TEMP t144,
 CONST 5)
MOVE(
 TEMP t145,
 CONST 6)
MOVE(
 TEMP t146,
 CONST 7)
MOVE(
 TEMP t146,
 BINOP(MINUS,
  TEMP t144,
  TEMP t145))
L25:
li t144, 5
li t145, 6
li t146, 7
sub t147,t144,t145
move t146, t147
j L24
L24:
