      $SET ACCEPTREFRESH

      *----------------------------------------------------------------*
       IDENTIFICATION       DIVISION.
      *----------------------------------------------------------------*
           PROGRAM-ID.       INCL-PROD.
           AUTHOR.           JULIANO NICOMEDES.
           DATE-WRITTEN.     15-11-2011.
      *----------------------------------------------------------------*
       ENVIRONMENT         DIVISION.
      *----------------------------------------------------------------*

      *--------------------------------
       INPUT-OUTPUT        SECTION.
      *--------------------------------

       FILE-CONTROL.
           SELECT ARQ-PROD ASSIGN TO "PRODUTOS.DAT"
               ORGANIZATION    IS INDEXED
               ACCESS MODE     IS RANDOM
               RECORD KEY      IS PROD-COD
               FILE STATUS     IS W-STATP.

      *----------------------------------------------------------------*
       DATA                DIVISION.
      *----------------------------------------------------------------*

      *--------------------------------
       FILE                SECTION.
      *--------------------------------
       FD  ARQ-PROD.
       01  REG-PROD.
           02 PROD-COD    PIC 9(4).
           02 PROD-NOME   PIC X(30).
           02 PROD-PRECO  PIC 999V99.
           02 PROD-QTD    PIC 99.
           02 FILLER      PIC X(80).

      *--------------------------------
       WORKING-STORAGE SECTION.
      *--------------------------------
       77 W-STATP         PIC X(02) VALUE SPACES.
       77 W-RESP          PIC X     VALUE SPACES.
       77 W-TRACO         PIC X(80) VALUE ALL "=".
       77 LIMPA           PIC X(30) VALUE SPACES.

       01 DATA-DO-DIA-INV.
           02 ANO-INV PIC 9(4).
           02 MES-INV PIC 9(2).
           02 DIA-INV PIC 9(2).
       01 DATA-DO-DIA.
           02 DIA PIC 99.
           02 FILLER PIC X VALUE "/".
           02 MES PIC 99.
           02 FILLER PIC X VALUE "/".
           02 ANO PIC 9(4).

      *--------------------------------
       SCREEN SECTION.
      *--------------------------------
       01 TELA-1 BACKGROUND-COLOR 7.
           05 BLANK SCREEN.
           05 MOLDURA FOREGROUND-COLOR 01.
               10 LINE 01 COLUMN 01 PIC X(80) FROM W-TRACO.
               10 LINE 03 COLUMN 01 PIC X(80) FROM W-TRACO.
           05 LINE 02 COLUMN 20 VALUE
               "C A D A S T R O   D E  P R O D U T O S"
               FOREGROUND-COLOR 04.
           05 TEXTOS-TELA FOREGROUND-COLOR 01.
               10 LINE 09 COLUMN 10 VALUE
               "COD..............:[    ]".
               10 LINE 11 COLUMN 10 VALUE
               "PRODUTO..........:[                              ]".
               10 LINE 13 COLUMN 10 VALUE
               "PRECO............:[       ]".
               10 LINE 15 COLUMN 10 VALUE
               "QUANTIDADE.......:[  ]".


           05 MOLDURA1 FOREGROUND-COLOR 01.
               10 LINE 22 COLUMN 01 PIC X(80) FROM W-TRACO.
               10 LINE 24 COLUMN 01 PIC X(80) FROM W-TRACO.

           05 LINE 23 COLUMN 03 VALUE "MENSAGEM ==>> "
               FOREGROUND-COLOR 4.
           05 DIA-DATA FOREGROUND-COLOR 04.
               10 LINE 23 COLUMN 63 VALUE "DATA: ".
               10 LINE 23 COLUMN 70 PIC X(10) FROM DATA-DO-DIA.



       01 TELA-2 FOREGROUND-COLOR 4 HIGHLIGHT.
           05 I1 LINE 09 COLUMN 29 PIC 9(4)     USING PROD-COD.
           05 I2 LINE 11 COLUMN 29 PIC X(30)    USING PROD-NOME.
           05 I3 LINE 13 COLUMN 29 PIC $ZZ9,99  USING PROD-PRECO.
           05 I4 LINE 15 COLUMN 29 PIC 99       USING PROD-QTD.

       01 MENSAGENS FOREGROUND-COLOR 04.
           05 M0 LINE 23 COLUMN 17 PIC X(30) FROM LIMPA.
           05 M1 LINE 23 COLUMN 17 VALUE "GRAVAR REGISTRO? (S/N)".
           05 M2 LINE 23 COLUMN 17 VALUE "CAMPO OBRIGATORIO!!!!!".
           05 M3 LINE 23 COLUMN 17 VALUE "CADASTRAR OUTRO? (S/N)".
           05 M4 LINE 23 COLUMN 17 VALUE "PRODUTO JA CADASTRADO!".
           05 MR LINE 23 COLUMN 41 PIC X TO W-RESP AUTO.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

       MAIN.
           PERFORM INICIO.
           PERFORM EXTRAI-DATA.
           PERFORM PROCESSA UNTIL W-RESP = "N".
           PERFORM FIM.
           STOP RUN.

       INICIO.
           OPEN INPUT ARQ-PROD.

       PROCESSA.

           INITIALIZE REG-PROD.
           DISPLAY TELA-1.
           DISPLAY M0.
           PERFORM ENTRA-COD.
           PERFORM CONTINUA.

       FIM.
           CLOSE ARQ-PROD.


       CONTINUA.
           DISPLAY M0
           DISPLAY M3
           PERFORM WITH TEST AFTER UNTIL W-RESP = "S" OR "N"
               ACCEPT MR
               MOVE FUNCTION UPPER-CASE (W-RESP) TO W-RESP
           END-PERFORM.


       EXTRAI-DATA.
           ACCEPT DATA-DO-DIA-INV FROM DATE YYYYMMDD.
           MOVE ANO-INV TO ANO.
           MOVE MES-INV TO MES.
           MOVE DIA-INV TO DIA.

       ENTRA-COD.
           PERFORM WITH TEST AFTER UNTIL PROD-COD NOT = ZEROS
               ACCEPT I1
               IF PROD-COD = ZEROS
                   DISPLAY M2
                   STOP " "
                   DISPLAY M0
               END-IF
               END-PERFORM.
               READ ARQ-PROD
               IF W-STATP = "23"
                   PERFORM ENTRA-RESTO
                   PERFORM GRAVACAO
               ELSE
                   DISPLAY M4
                   STOP " "
                   DISPLAY M0
               END-IF.



       ENTRA-RESTO.
           PERFORM ENTRA-I2.
           PERFORM ENTRA-I3.
           PERFORM ENTRA-I4.


       ENTRA-I2.
           PERFORM WITH TEST AFTER UNTIL PROD-NOME NOT = SPACES
               ACCEPT I2
               IF PROD-NOME = SPACES
                   DISPLAY M2
                   STOP " "
                   DISPLAY M0
               END-IF
               END-PERFORM.

       ENTRA-I3.
           PERFORM WITH TEST AFTER UNTIL PROD-PRECO NOT = ZEROS
               ACCEPT I3
               IF PROD-PRECO = ZEROS
                   DISPLAY M2
                   STOP " "
                   DISPLAY M0
               END-IF
           END-PERFORM.

       ENTRA-I4.
           PERFORM WITH TEST AFTER UNTIL PROD-QTD NOT = SPACES
               ACCEPT I4
               IF PROD-QTD = ZEROS
                   DISPLAY M2
                   STOP " "
                   DISPLAY M0
               END-IF
               END-PERFORM.


       GRAVACAO.
           DISPLAY M1
           PERFORM WITH TEST AFTER UNTIL W-RESP = "S" OR "N"
               ACCEPT MR
               MOVE FUNCTION UPPER-CASE (W-RESP) TO W-RESP
           END-PERFORM.

           IF W-RESP = "S"
               MOVE FUNCTION UPPER-CASE(REG-PROD) TO REG-PROD
               WRITE REG-PROD
           END-IF.















