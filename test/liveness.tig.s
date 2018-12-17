SEQ(
 MOVE(
  MEM(
   BINOP(PLUS,
    TEMP t131,
    CONST 0)),
  TEMP t104),
 SEQ(
  MOVE(
   MEM(
    BINOP(PLUS,
     TEMP t131,
     CONST 4)),
   TEMP t130),
  SEQ(
   MOVE(
    MEM(
     BINOP(PLUS,
      TEMP t131,
      CONST 8)),
    TEMP t116),
   SEQ(
    MOVE(
     MEM(
      BINOP(PLUS,
       TEMP t131,
       CONST 12)),
     TEMP t117),
    SEQ(
     MOVE(
      MEM(
       BINOP(PLUS,
        TEMP t131,
        CONST 16)),
      TEMP t118),
     SEQ(
      MOVE(
       MEM(
        BINOP(PLUS,
         TEMP t131,
         CONST 20)),
       TEMP t119),
      SEQ(
       MOVE(
        MEM(
         BINOP(PLUS,
          TEMP t131,
          CONST 24)),
        TEMP t120),
       SEQ(
        MOVE(
         MEM(
          BINOP(PLUS,
           TEMP t131,
           CONST 28)),
         TEMP t121),
        SEQ(
         MOVE(
          MEM(
           BINOP(PLUS,
            TEMP t131,
            CONST 32)),
          TEMP t122),
         SEQ(
          MOVE(
           MEM(
            BINOP(PLUS,
             TEMP t131,
             CONST 36)),
           TEMP t123),
          SEQ(
           MOVE(
            TEMP t102,
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
                    CONST 0))))))))),
           SEQ(
            MOVE(
             TEMP t116,
             MEM(
              BINOP(PLUS,
               TEMP t131,
               CONST 8))),
            SEQ(
             MOVE(
              TEMP t117,
              MEM(
               BINOP(PLUS,
                TEMP t131,
                CONST 12))),
             SEQ(
              MOVE(
               TEMP t118,
               MEM(
                BINOP(PLUS,
                 TEMP t131,
                 CONST 16))),
              SEQ(
               MOVE(
                TEMP t119,
                MEM(
                 BINOP(PLUS,
                  TEMP t131,
                  CONST 20))),
               SEQ(
                MOVE(
                 TEMP t120,
                 MEM(
                  BINOP(PLUS,
                   TEMP t131,
                   CONST 24))),
                SEQ(
                 MOVE(
                  TEMP t121,
                  MEM(
                   BINOP(PLUS,
                    TEMP t131,
                    CONST 28))),
                 SEQ(
                  MOVE(
                   TEMP t122,
                   MEM(
                    BINOP(PLUS,
                     TEMP t131,
                     CONST 32))),
                  SEQ(
                   MOVE(
                    TEMP t123,
                    MEM(
                     BINOP(PLUS,
                      TEMP t131,
                      CONST 36))),
                   MOVE(
                    TEMP t130,
                    MEM(
                     BINOP(PLUS,
                      TEMP t131,
                      CONST 4))))))))))))))))))))))
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 0)),
 TEMP t104)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 4)),
 TEMP t130)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 8)),
 TEMP t116)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 12)),
 TEMP t117)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 16)),
 TEMP t118)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 20)),
 TEMP t119)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 24)),
 TEMP t120)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 28)),
 TEMP t121)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 32)),
 TEMP t122)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 36)),
 TEMP t123)
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
MOVE(
 TEMP t102,
 CONST 0)
MOVE(
 TEMP t116,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 8)))
MOVE(
 TEMP t117,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 12)))
MOVE(
 TEMP t118,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 16)))
MOVE(
 TEMP t119,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 20)))
MOVE(
 TEMP t120,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 24)))
MOVE(
 TEMP t121,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 28)))
MOVE(
 TEMP t122,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 32)))
MOVE(
 TEMP t123,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 36)))
MOVE(
 TEMP t130,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 4)))
L11:
sw $fp, 0($sp)
move $fp, $sp
addiu $sp, $sp, 40
L16:
sw $s2, 0($s1)
sw $v1, 4($s1)
sw $at, 8($s1)
sw $k0, 12($s1)
sw $k1, 16($s1)
sw $gp, 20($s1)
sw $sp, 24($s1)
sw $fp, 28($s1)
sw $ra, 32($s1)
sw $a0, 36($s1)
li $a3, 5
li $a1, 0
L13:
li $s2, 10
blt $a1,$s2,L14
L12:
li $s2, 0
lw $at, 8($s1)
lw $k0, 12($s1)
lw $k1, 16($s1)
lw $gp, 20($s1)
lw $sp, 24($s1)
lw $fp, 28($s1)
lw $ra, 32($s1)
lw $a0, 36($s1)
lw $v1, 4($s1)
j L15
L14:
addi $s0,$a1,1
move $a1, $s0
add $a3,$a3,$a1
move $a3, $a3
li $a2, 2
mul $a1,$a1,$a2
move $a1, $a1
j L13
L15:
move $sp, $fp
lw $fp, 0($sp)
jr $ra
