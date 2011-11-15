      *-----------------------------------------------------------------*
       IDENTIFICATION          DIVISION.
      *-----------------------------------------------------------------*

       PROGRAM-ID.             SOMAR.
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

       77 RESULTADO PIC 9(5) VALUE ZEROS.

       LINKAGE SECTION.
       01 CAMP1 PIC 9(2).
       01 CAMP2 PIC 9(3).
       01 CAMP3 PIC 9(5).

      *-----------------------------------------------------------------*
       PROCEDURE DIVISION USING CAMP1 CAMP2 CAMP3.
      *-----------------------------------------------------------------*
       INICIO.
           COMPUTE RESULTADO = CAMP1 + CAMP2.
           MOVE RESULTADO TO CAMP3
           EXIT PROGRAM.
