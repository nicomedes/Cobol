      *-----------------------------------------------------------------*
       IDENTIFICATION          DIVISION.
      *-----------------------------------------------------------------*

       PROGRAM-ID.             PRECOFABRI.
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

       01 VALOR-VEICULO-ED PIC Z(10),9(2) VALUE ZEROS.
       01 TAXA-DISTRIBUIDOR PIC 9V9(3) VALUE 0,28.
       01 TAXA-IMPOSTOS PIC 9(10)V9(2) VALUE 0,45.
       01 VALOR-VEICULO PIC 9(10)V99 VALUE ZEROS.

      *-----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------*
       INICIO.
           ACCEPT VALOR-VEICULO-ED.
           MOVE VALOR-VEICULO-ED TO VALOR-VEICULO.
           COMPUTE VALOR-VEICULO = VALOR-VEICULO + (VALOR-VEICULO *
           TAXA-IMPOSTOS).
           COMPUTE VALOR-VEICULO = VALOR-VEICULO + (VALOR-VEICULO *
           TAXA-DISTRIBUIDOR).
           MOVE VALOR-VEICULO TO VALOR-VEICULO-ED.
           DISPLAY VALOR-VEICULO-ED.
       STOP-RUN.

