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
li t132, 5
li t133, 0
L13:
li t135, 10
blt t133,t135,L14
L12:
j L15
L14:
addi t136,t133,1
move t134, t136
add t137,t132,t134
move t132, t137
li t139, 2
mul t138,t134,t139
move t133, t138
j L13
L15:
