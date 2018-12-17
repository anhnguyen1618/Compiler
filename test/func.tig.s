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
             EXP(
              CONST 0),
             ESEQ(
              EXP(
               CONST 0),
              CALL(
               NAME L12,
                TEMP t131,
                CONST 0,
                NAME L20)))),
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
 TEMP t102,
 CALL(
  NAME L12,
   TEMP t131,
   CONST 0,
   NAME L20))
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
addiu $sp, $sp, 52
L22:
sw $a0, 0($fp)
sw $ra, 4($fp)
sw $s0, 8($fp)
sw $s1, 12($fp)
sw $s2, 16($fp)
sw $s3, 20($fp)
sw $s4, 24($fp)
sw $s5, 28($fp)
sw $s6, 32($fp)
sw $s7, 36($fp)
move $a0, $fp
li t134, 0
move $a1, t134
la t135, L20
move $a2, t135
jal L12
move $v0, $v0
lw $s0, 8($fp)
lw $s1, 12($fp)
lw $s2, 16($fp)
lw $s3, 20($fp)
lw $s4, 24($fp)
lw $s5, 28($fp)
lw $s6, 32($fp)
lw $s7, 36($fp)
lw $ra, 4($fp)
j L21
L21:
move $sp, $fp
lw $fp, 0($sp)
jr $ra
L20 .asciiz "str2"
SEQ(
 MOVE(
  MEM(
   BINOP(PLUS,
    TEMP t131,
    CONST 0)),
  TEMP t104),
 SEQ(
  MOVE(
   TEMP t133,
   TEMP t105),
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
               EXP(
                CALL(
                 NAME L12,
                  MEM(
                   TEMP t131),
                  MEM(
                   BINOP(PLUS,
                    TEMP t131,
                    CONST 0)),
                  NAME L18)),
               EXP(
                CONST 0)),
              NAME L19)),
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
                       CONST 4)))))))))))))))))))))))
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 0)),
 TEMP t104)
MOVE(
 TEMP t133,
 TEMP t105)
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
EXP(
 CALL(
  NAME L12,
   MEM(
    TEMP t131),
   MEM(
    BINOP(PLUS,
     TEMP t131,
     CONST 0)),
   NAME L18))
MOVE(
 TEMP t102,
 NAME L19)
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
L17:
sw $fp, 0($sp)
move $fp, $sp
addiu $sp, $sp, 52
L24:
sw $a0, 0($fp)
move t133, $a1
sw $ra, 4($fp)
sw $s0, 8($fp)
sw $s1, 12($fp)
sw $s2, 16($fp)
sw $s3, 20($fp)
sw $s4, 24($fp)
sw $s5, 28($fp)
sw $s6, 32($fp)
sw $s7, 36($fp)
lw t136, 0($fp)
move $a0, t136
lw t137, 0($fp)
move $a1, t137
la t138, L18
move $a2, t138
jal L12
la $v0, L19
lw $s0, 8($fp)
lw $s1, 12($fp)
lw $s2, 16($fp)
lw $s3, 20($fp)
lw $s4, 24($fp)
lw $s5, 28($fp)
lw $s6, 32($fp)
lw $s7, 36($fp)
lw $ra, 4($fp)
j L23
L23:
move $sp, $fp
lw $fp, 0($sp)
jr $ra
L19 .asciiz " "
L18 .asciiz "str"
SEQ(
 MOVE(
  MEM(
   BINOP(PLUS,
    TEMP t131,
    CONST 0)),
  TEMP t104),
 SEQ(
  MOVE(
   TEMP t132,
   TEMP t105),
  SEQ(
   MOVE(
    MEM(
     BINOP(PLUS,
      TEMP t131,
      CONST 4)),
    TEMP t106),
   SEQ(
    MOVE(
     MEM(
      BINOP(PLUS,
       TEMP t131,
       CONST 8)),
     TEMP t130),
    SEQ(
     MOVE(
      MEM(
       BINOP(PLUS,
        TEMP t131,
        CONST 12)),
      TEMP t116),
     SEQ(
      MOVE(
       MEM(
        BINOP(PLUS,
         TEMP t131,
         CONST 16)),
       TEMP t117),
      SEQ(
       MOVE(
        MEM(
         BINOP(PLUS,
          TEMP t131,
          CONST 20)),
        TEMP t118),
       SEQ(
        MOVE(
         MEM(
          BINOP(PLUS,
           TEMP t131,
           CONST 24)),
         TEMP t119),
        SEQ(
         MOVE(
          MEM(
           BINOP(PLUS,
            TEMP t131,
            CONST 28)),
          TEMP t120),
         SEQ(
          MOVE(
           MEM(
            BINOP(PLUS,
             TEMP t131,
             CONST 32)),
           TEMP t121),
          SEQ(
           MOVE(
            MEM(
             BINOP(PLUS,
              TEMP t131,
              CONST 36)),
            TEMP t122),
           SEQ(
            MOVE(
             MEM(
              BINOP(PLUS,
               TEMP t131,
               CONST 40)),
             TEMP t123),
            SEQ(
             MOVE(
              TEMP t102,
              ESEQ(
               SEQ(
                EXP(
                 CALL(
                  NAME L13,
                   MEM(
                    TEMP t131),
                   BINOP(PLUS,
                    MEM(
                     BINOP(PLUS,
                      TEMP t131,
                      CONST 0)),
                    CONST 1))),
                EXP(
                 CONST 0)),
               CONST 0)),
             SEQ(
              MOVE(
               TEMP t116,
               MEM(
                BINOP(PLUS,
                 TEMP t131,
                 CONST 12))),
              SEQ(
               MOVE(
                TEMP t117,
                MEM(
                 BINOP(PLUS,
                  TEMP t131,
                  CONST 16))),
               SEQ(
                MOVE(
                 TEMP t118,
                 MEM(
                  BINOP(PLUS,
                   TEMP t131,
                   CONST 20))),
                SEQ(
                 MOVE(
                  TEMP t119,
                  MEM(
                   BINOP(PLUS,
                    TEMP t131,
                    CONST 24))),
                 SEQ(
                  MOVE(
                   TEMP t120,
                   MEM(
                    BINOP(PLUS,
                     TEMP t131,
                     CONST 28))),
                  SEQ(
                   MOVE(
                    TEMP t121,
                    MEM(
                     BINOP(PLUS,
                      TEMP t131,
                      CONST 32))),
                   SEQ(
                    MOVE(
                     TEMP t122,
                     MEM(
                      BINOP(PLUS,
                       TEMP t131,
                       CONST 36))),
                    SEQ(
                     MOVE(
                      TEMP t123,
                      MEM(
                       BINOP(PLUS,
                        TEMP t131,
                        CONST 40))),
                     MOVE(
                      TEMP t130,
                      MEM(
                       BINOP(PLUS,
                        TEMP t131,
                        CONST 8))))))))))))))))))))))))
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 0)),
 TEMP t104)
MOVE(
 TEMP t132,
 TEMP t105)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 4)),
 TEMP t106)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 8)),
 TEMP t130)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 12)),
 TEMP t116)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 16)),
 TEMP t117)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 20)),
 TEMP t118)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 24)),
 TEMP t119)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 28)),
 TEMP t120)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 32)),
 TEMP t121)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 36)),
 TEMP t122)
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 40)),
 TEMP t123)
EXP(
 CALL(
  NAME L13,
   MEM(
    TEMP t131),
   BINOP(PLUS,
    MEM(
     BINOP(PLUS,
      TEMP t131,
      CONST 0)),
    CONST 1)))
MOVE(
 TEMP t102,
 CONST 0)
MOVE(
 TEMP t116,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 12)))
MOVE(
 TEMP t117,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 16)))
MOVE(
 TEMP t118,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 20)))
MOVE(
 TEMP t119,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 24)))
MOVE(
 TEMP t120,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 28)))
MOVE(
 TEMP t121,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 32)))
MOVE(
 TEMP t122,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 36)))
MOVE(
 TEMP t123,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 40)))
MOVE(
 TEMP t130,
 MEM(
  BINOP(PLUS,
   TEMP t131,
   CONST 8)))
L15:
sw $fp, 0($sp)
move $fp, $sp
addiu $sp, $sp, 52
L26:
sw $a0, 0($fp)
move t132, $a1
sw $a2, 4($fp)
sw $ra, 8($fp)
sw $s0, 12($fp)
sw $s1, 16($fp)
sw $s2, 20($fp)
sw $s3, 24($fp)
sw $s4, 28($fp)
sw $s5, 32($fp)
sw $s6, 36($fp)
sw $s7, 40($fp)
lw t139, 0($fp)
move $a0, t139
lw t141, 0($fp)
addi t140,t141,1
move $a1, t140
jal L13
li $v0, 0
lw $s0, 12($fp)
lw $s1, 16($fp)
lw $s2, 20($fp)
lw $s3, 24($fp)
lw $s4, 28($fp)
lw $s5, 32($fp)
lw $s6, 36($fp)
lw $s7, 40($fp)
lw $ra, 8($fp)
j L25
L25:
move $sp, $fp
lw $fp, 0($sp)
jr $ra
