      *-----------------------------------------------------------------*
       IDENTIFICATION          DIVISION.
      *-----------------------------------------------------------------*

       PROGRAM-ID.             TAXA.
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

       01 VALOR-DEPOSIT-ED PIC Z(10),9(2) VALUE ZEROS.
       01 TAXA-JUROS PIC 9V9(3) VALUE 0,007.
       01 VALOR-DEPOSIT PIC 9(10)V9(2) VALUE ZEROS.
       01 TAXA-JUROS-ED PIC 9,999 VALUE ZEROS.

      *-----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------*

       INICIO.
           ACCEPT VALOR-DEPOSIT-ED.
           MOVE VALOR-DEPOSIT-ED TO VALOR-DEPOSIT.
           COMPUTE VALOR-DEPOSIT = VALOR-DEPOSIT +
           (VALOR-DEPOSIT * TAXA-JUROS).
           MOVE VALOR-DEPOSIT TO VALOR-DEPOSIT-ED.
           MOVE TAXA-JUROS TO TAXA-JUROS-ED.
           DISPLAY VALOR-DEPOSIT-ED.
           DISPLAY TAXA-JUROS-ED.

       STOP-RUN.


