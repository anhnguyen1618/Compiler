EXP(
 ESEQ(
  SEQ(
   MOVE(
    TEMP t132,
    CONST 5),
   MOVE(
    TEMP t133,
    CONST 0)),
  ESEQ(
   EXP(
    CONST 0),
   ESEQ(
    JUMP(
     NAME L13),
    ESEQ(
     LABEL L13,
     ESEQ(
      CJUMP(LT,
       TEMP t133,
       CONST 10,
       L14,L12),
      ESEQ(
       LABEL L14,
       ESEQ(
        EXP(
         ESEQ(
          MOVE(
           TEMP t134,
           BINOP(PLUS,
            TEMP t133,
            CONST 1)),
          ESEQ(
           SEQ(
            MOVE(
             TEMP t132,
             BINOP(PLUS,
              TEMP t132,
              TEMP t134)),
            EXP(
             CONST 0)),
           ESEQ(
            MOVE(
             TEMP t133,
             BINOP(MUL,
              TEMP t134,
              CONST 2)),
            CONST 0)))),
        ESEQ(
         SEQ(
          JUMP(
           NAME L13),
          LABEL L12),
         CONST 0)))))))))
MOVE(
 TEMP t132,
 CONST 5)
MOVE(
 TEMP t133,
 CONST 0)
JUMP(
 NAME L13)
LABEL L13
CJUMP(LT,
 TEMP t133,
 CONST 10,
 L14,L12)
LABEL L14
MOVE(
 TEMP t134,
 BINOP(PLUS,
  TEMP t133,
  CONST 1))
MOVE(
 TEMP t132,
 BINOP(PLUS,
  TEMP t132,
  TEMP t134))
MOVE(
 TEMP t133,
 BINOP(MUL,
  TEMP t134,
  CONST 2))
JUMP(
 NAME L13)
LABEL L12
L16:
li $zero, 5
li $v0, 0
L13:
li $at, 10
blt $v0,$at,L14
L12:
j L15
L14:
addi $at,$v0,1
move $v0, $at
add $zero,$zero,$v0
move $zero, $zero
li $v1, 2
mul $v0,$v0,$v1
move $v0, $v0
j L13
L15:
