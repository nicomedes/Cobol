      $SET ACCEPTREFRESH
       IDENTIFICATION        DIVISION.
           PROGRAM-ID.       CADPAC.
           AUTHOR.           SOLANGE BARROS.
           DATE-WRITTEN.     22-07-2010.

       ENVIRONMENT         DIVISION.

       INPUT-OUTPUT        SECTION.

       FILE-CONTROL.
           SELECT ARQ-PAC ASSIGN TO "PACIENTE.DAT"
               FILE STATUS IS W-STATP.

       DATA                DIVISION.

       FILE                SECTION.
       FD                  ARQ-PAC.
       01                  REG-PAC.
           05 NOME-PAC   PIC X(30).
           05 IDADE-PAC  PIC 9(02).
           05 FONE-PAC   PIC X(09).
           05 DTNASC-PAC PIC X(10).
           05 CONV-PAC   PIC X(02).

       WORKING-STORAGE SECTION.
       77 W-STATP        PIC X(02) VALUE   SPACES.
       77 W-RESP         PIC X     VALUE   SPACES.
       77 W-TRACO        PIC X(80) VALUE   ALL "=".

       SCREEN SECTION.
       01 TELA-1 BACKGROUND-COLOR 7.
           05 BLANK SCREEN.
           05 MOLDURA FOREGROUND-COLOR 01.
               10 LINE 01 COLUMN 01 VALUE "E".
               10 LINE 01 COLUMN 02 PIC X(78) FROM W-TRACO.
               10 LINE 01 COLUMN 80 VALUE ">".
               10 LINE 02 COLUMN 01 VALUE "o".
               10 LINE 02 COLUMN 80 VALUE "o".
               10 LINE 03 COLUMN 01 VALUE "E".
               10 LINE 03 COLUMN 02 PIC X(78) FROM W-TRACO.
               10 LINE 03 COLUMN 80 VALUE "U".
           05 LINE 02 COLUMN 20 VALUE
               "C A D A S T R O   D E  P A C I E N T E S"
               FOREGROUND-COLOR 04.
           05 TEXTOS-TELA FOREGROUND-COLOR 01.
               10 LINE 09 COLUMN 10 VALUE
               "NOME DO PACIENTE.:[                              ]".
               10 LINE 11 COLUMN 10 VALUE
               "IDADE............:[  ]".
               10 LINE 13 COLUMN 10 VALUE
               "TELEFONE.........:[         ]".
               10 LINE 15 COLUMN 10 VALUE
               "DATA NASCIMENTO..:[          ]".
               10 LINE 17 COLUMN 10 VALUE
               "CONVENIO.........:[  ]".
           05 MOLDURA1 FOREGROUND-COLOR 01.
               10 LINE 22 COLUMN 01 VALUE "E".
               10 LINE 22 COLUMN 02 PIC X(78) FROM W-TRACO.
               10 LINE 22 COLUMN 80 VALUE ">".
               10 LINE 23 COLUMN 01 VALUE "o".
               10 LINE 23 COLUMN 80 VALUE "o".
               10 LINE 24 COLUMN 01 VALUE "E".
               10 LINE 24 COLUMN 02 PIC X(78) FROM W-TRACO.
               10 LINE 24 COLUMN 80 VALUE "U".
           05 LINE 23 COLUMN 03 VALUE "MENSAGEM ==>> "
               FOREGROUND-COLOR 4.

       01 TELA-2 FOREGROUND-COLOR 4.
           05 I1 LINE 09 COLUMN 29 PIC X(30) USING NOME-PAC.
           05 I2 LINE 11 COLUMN 29 PIC 9(2)  USING IDADE-PAC AUTO.
           05 I3 LINE 13 COLUMN 29 PIC X(09) USING FONE-PAC.
           05 I4 LINE 15 COLUMN 29 PIC X(10) USING DTNASC-PAC.
           05 I5 LINE 17 COLUMN 29 PIC X(02) USING CONV-PAC.
       01 TELA-3 FOREGROUND-COLOR 04.
           05 LINE 23 COLUMN 25 VALUE
           "CONTINUA CADASTRAMENTO? (S/N)".
           05 LINE 23 COLUMN 65 PIC X TO W-RESP AUTO.

       PROCEDURE DIVISION.

       MESTRA.
           PERFORM INICIO
           PERFORM PROCESSA UNTIL W-RESP = "N"
           PERFORM FIM
           STOP RUN.
       INICIO.
           OPEN EXTEND ARQ-PAC.
       PROCESSA.
           INITIALIZE REG-PAC
           DISPLAY TELA-1
           PERFORM ENTRA-I1
           PERFORM ENTRA-I2
           PERFORM ENTRA-I3
           PERFORM ENTRA-I4
           PERFORM ENTRA-I5
           WRITE REG-PAC
           PERFORM CONTINUA.
       FIM.
           CLOSE ARQ-PAC.

       ENTRA-I1.
           PERFORM WITH TEST AFTER UNTIL NOME-PAC NOT = SPACES
               ACCEPT I1
           END-PERFORM.
       ENTRA-I2.
           ACCEPT I2.
       ENTRA-I3.
           PERFORM WITH TEST AFTER UNTIL FONE-PAC NOT = SPACES
               ACCEPT I3
           END-PERFORM.
       ENTRA-I4.
           PERFORM WITH TEST AFTER UNTIL DTNASC-PAC NOT = SPACES
               ACCEPT I4
           END-PERFORM.
       ENTRA-I5.
           ACCEPT I5.

       CONTINUA.
           DISPLAY TELA-3
           PERFORM WITH TEST AFTER UNTIL W-RESP = "S" OR "N"
               ACCEPT TELA-3
               MOVE FUNCTION UPPER-CASE (W-RESP) TO W-RESP
           END-PERFORM.

