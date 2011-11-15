      *-----------------------------------------------------------------*
       IDENTIFICATION          DIVISION.
      *-----------------------------------------------------------------*

       PROGRAM-ID.             PGMENU.
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
       77  OPC PIC X(1)    VALUE   SPACE.
       77  BRANCO  PIC X(20) VALUE SPACES.
       01 MODULO1 PIC X(7) VALUE "ENTRADA".
       01 MODULO2 PIC X(5) VALUE "SAIDA".

       SCREEN SECTION.
       01 TELA.
           02 BLANK SCREEN.
           02 LINE 05 COLUMN 30 VALUE "MENU".
           02 LINE 10 COLUMN 10 VALUE
               "(1) RECEBIMENTO DE MERCADORIA - ENTRADA".
           02 LINE 12 COLUMN 10 VALUE
               "(2) SAIDA DE MERCADORIA - SAIDA".
           02 LINE 15 COLUMN 10 VALUE "(9) ENCERRAR".
           02 LINE 20 COLUMN 20 VALUE "ESCOLHA A OPCAO:".
           02 LINHA-OPC LINE 20 COLUMN 40 PIC X TO OPC.
       01 LIMPA.
           02 LINE 20 COLUMN 50 PIC X(20) FROM BANCO.



      *-----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------*
       INICIO.
           PERFORM ROT-PROCESSA UNTIL OPC = "9".
           STOP RUN.
       ROT-PROCESSA.
           DISPLAY TELA.
           PERFORM WITH TEST AFTER UNTIL OPC = "1" OR "2" OR "9"
               ACCEPT LINHA-OPC
               IF OPC = "1" OR "2" OR "9"
                   DISPLAY LIMPA
               ELSE
                   DISPLAY "DIGITE 1 OU 2 OU 9" AT 2050
               END-IF
           END-PERFORM.

           EVALUATE OPC
               WHEN 1
                   CALL "MODULO1"
               WHEN 2
                   CALL "MODULO2"
           END-EVALUATE.
