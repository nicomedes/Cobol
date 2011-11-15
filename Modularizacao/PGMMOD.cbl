      *-----------------------------------------------------------------*
       IDENTIFICATION          DIVISION.
      *-----------------------------------------------------------------*

       PROGRAM-ID.             PGMMOD.
       AUTHOR.                 JULIANO NICOMEDES.
       INSTALLATION.           UNIVERSIDADE PRESBITERIANA MACKENZIE.
       SECURITY.               PROGRAMA DE USO EXCLUSIVO DOS ALUNOS.

      *-----------------------------------------------------------------*
       ENVIRONMENT             DIVISION.
      *-----------------------------------------------------------------*

       CONFIGURATION           SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------*
       DATA                    DIVISION.
      *-----------------------------------------------------------------*

       WORKING-STORAGE         SECTION.

       01  VAR1 PIC 9(2) VALUE 20.
       01  VAR2 PIC 9(3) VALUE 100.
       01  VAR3 PIC 9(5) VALUE ZEROS.

      *-----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------*
       INICIO.
           CALL "SOMAR" USING VAR1 VAR2 VAR3.
           DISPLAY ERASE.
           DISPLAY "RESULTADO DA SOMA = " AT 1020.
           DISPLAY VAR3 AT 1042.
           STOP RUN.
