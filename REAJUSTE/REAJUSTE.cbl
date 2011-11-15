      *-----------------------------------------------------------------*
       IDENTIFICATION          DIVISION.
      *-----------------------------------------------------------------*

       PROGRAM-ID.             REAJUSTE.
       AUTHOR.                 JULIANO NICOMEDES.
       INSTALLATION.       UNIVERSIDADE PRESBITERIANA MACKENZIE.
       SECURITY.           PROGRAMA DE USO EXCLUSIVO DOS ALUNOS.

      *-----------------------------------------------------------------*
       ENVIRONMENT             DIVISION.
      *-----------------------------------------------------------------*

       CONFIGURATION           SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------*
       DATA                    DIVISION.
      *-----------------------------------------------------------------*

       WORKING-STORAGE         SECTION.

       01 CONT PIC 9(3) VALUE ZEROS.
       01 SALARIO-MINIMO-ED PIC 9(10),99 VALUE ZEROS.
       01 SALARIO-MINIMO PIC 9(10)V99 VALUE ZEROS.
       01 TOTAL PIC 9(10),99 VALUE ZEROS.
       01 SALARIO-ATUAL-ED PIC 9(10),99 VALUE ZEROS.


       01 FUNCIONARIO.
           03 FUNC OCCURS 584 TIMES.
               05 NOME-FUNC PIC X(30).
               05 SALARIO-ATUAL PIC 9(10)V99 VALUE ZEROS.


      *-----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------*

       INICIO.
           ADD 1 TO COUNT
           ACCEPT SALARIO-MINIMO-ED.
           MOVE SALARIO-MINIMO-ED TO SALARIO-MINIMO.

           PERFORM UNTIL CONT > 585
               ACCEPT NOME-FUNC(CONT)
               ACCEPT SALARIO-ATUAL-ED.
               MOVE SALARIO-ATUAL-ED TO SALARIO-ATUAL(CONT).
               EVALUATE TRUE
                   WHEN SALARIO-ATUAL(CONT) < SALARIO-MINIMO * 3
                   COMPUTE SALARIO-ATUAL(CONT) = SALARIO-ATUAL(CONT) +
                   (SALARIO-ATUAL(CONT) * 0,50)
                   COMPUTE TOTAL = TOTAL + (SALARIO-ATUAL(CONT)* 0,50
                   WHEN SALARIO-ATUAL(CONT) > 3 * SALARIO-MINIMO AND >=
                   10 * SALARIO-MINIMO
                   s

