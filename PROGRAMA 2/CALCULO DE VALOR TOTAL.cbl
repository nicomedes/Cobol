       IDENTIFICATION          DIVISION.
       PROGRAM-ID.              EDICAO.
       AUTHOR.                  TAKATO.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       SPECIAL-NAMES.          DECIMAL-POINT IS COMMA.

       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
       77  QUANTIDADE      PIC 9(4)        VALUE ZEROS.
       77  QUANTIDADE-ED   PIC -Z.ZZ9       VALUE ZEROS.
       77  PRECO           PIC 9(4)V99     VALUE ZEROS.
       77  PRECO-ED        PIC -Z.ZZ9,99    VALUE ZEROS.
       77  VAL-TOT         PIC 9(7)V99     VALUE ZEROS.
       77  VAL-TOT-ED      PIC -Z.ZZZ.ZZ9,99 VALUE ZEROS.
       77  X               PIC X           VALUE SPACES.

       PROCEDURE DIVISION.
       INICIO.
       MOVE ZERO TO QUANTIDADE, QUANTIDADE-ED, PRECO,
       PRECO-ED,VAL-TOT-ED, VAL-TOT.
       MOVE SPACES TO X.
       DISPLAY ERASE.
       DISPLAY "CALCULO DE VALOR TOTAL"    AT  0510.
       DISPLAY "QUANTIDADE:"               AT  1010.
       DISPLAY "(DE 1 A 4 DIGITOS)"        AT  1050.
       ACCEPT  QUANTIDADE-ED               AT  1041.
       MOVE    QUANTIDADE-ED   TO  QUANTIDADE.
       DISPLAY "PRECO UNITARIO:"           AT  1210.
       DISPLAY "(ATE 4 DIG. INTEIROS E 2 DEC.)"    AT 1250.
       ACCEPT PRECO-ED                     AT  1238.
       MOVE PRECO-ED TO PRECO.
       COMPUTE VAL-TOT = QUANTIDADE * PRECO.
       MOVE VAL-TOT TO VAL-TOT-ED.
       DISPLAY "VALOR TOTAL CALCULADO:"    AT  1610.
       DISPLAY VAL-TOT-ED  AT  1634.
       ACCEPT  X   AT 2034.
       IF X = "X"
           GO TO INICIO.

       DISPLAY ERASE.

       STOP RUN.

