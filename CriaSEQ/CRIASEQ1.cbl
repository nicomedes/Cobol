      $SET   ACCEPTREFRESH
       IDENTIFICATION       DIVISION.
       PROGRAM-ID.         CRIASEQ1.
       AUTHOR.        TAKATO.
       DATE-WRITTEN.    04 OUTUBRO 2011.
       ENVIRONMENT     DIVISION.
       CONFIGURATION    SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT  IS  COMMA.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
           SELECT   ARQ-DADOS    ASSIGN  TO   "DADOS1.DAT"
               ORGANIZATION  LINE  SEQUENTIAL
               FILE   STATUS    COD-ERRO.
       DATA        DIVISION.
       FILE        SECTION.
       FD    ARQ-DADOS
           LABEL   RECORD   STANDARD.
       01    REG-DADOS.
           02    CODIGO    		PIC  9(3).
           02    VALOR        	PIC  9(4)V99.
           02    FILLER        	PIC  X(51).
       WORKING-STORAGE     SECTION.
       77    COD-ERRO    PIC   X(2)   VALUE   SPACES.
       01    VARIAVEIS.
			02    W-CODIGO   	PIC    9(3)	VALUE   ZEROS.
			02    W-VALOR    	PIC    9(4)V99        VALUE   ZEROS.
				88    W-VALOR-OK VALUE  5,00 THRU  5000,00.
           02    W-GRAVA    PIC    X        VALUE   SPACE.
           02    W-OPC        PIC    X        VALUE   SPACE.
           88    W-OPC-OK      VALUE  "S"  "N".

       SCREEN        SECTION.
       01    TELA.
           02    BLANK   SCREEN.
           02    T1  LINE  05  COLUMN  20  VALUE  "ENTRADA DE DADOS".
           02    T2  LINE  10  COLUMN  10  VALUE  "CODIGO:  ".
           02    T3  LINE  12  COLUMN  10  VALUE  "VALOR:  ".
           02    T4  LINE  16  COLUMN  10  VALUE
                                      "CONFIRMA GRAVACAO?(S /N):  ".
           02    T5  LINE  20  COLUMN  10  VALUE
                                      "OUTRO  REGISTRO?(S/N):  ".
       01    TELA-DADOS.
           02    D-COD LINE 10 COLUMN 30  PIC ZZ9 TO  W-CODIGO REQUIRED.
           02    D-VAL LINE  12  COLUMN 30  PIC Z.ZZ9,99  TO  W-VALOR.
           02    D-GRAVA LINE 16 COLUMN 50  PIC X TO W-GRAVA  AUTO-SKIP.
           02    D-OPC  LINE  20 COLUMN 50  PIC X TO  W-OPC AUTO-SKIP.
       PROCEDURE      DIVISION.
       INICIO.
           PERFORM   ROT-ABRIR.
           PERFORM  ROT-PROCESSA  UNTIL  W-OPC  =  "N".
           PERFORM  ROT-FECHAR.
           DISPLAY   ERASE.
           DISPLAY  "FIM  DE  PROCESSAMENTO"  AT  0520.
           STOP   RUN.
       ROT-ABRIR.
           OPEN    EXTEND   ARQ-DADOS.
       ROT-PROCESSA.
           INITIALIZE   REG-DADOS.
           INITIALIZE   VARIAVEIS.
           PERFORM    FORMATAR-TELA.
           PERFORM   RECEBER-DADOS.
           PERFORM  GRAVAR-DADOS.
           PERFORM  RECEBER-OPC.
       FORMATAR-TELA.
           DISPLAY    TELA.
       RECEBER-DADOS.


           ACCEPT   D-COD.
           ACCEPT   D-VAL.

           PERFORM     WITH  TEST   AFTER  UNTIL  W-VALOR-OK
               ACCEPT  D-VAL
               IF   W-VALOR-OK
                   DISPLAY "                    "    AT  1655
               ELSE
                   DISPLAY " > 5,00 E  < 5000,00"
                   AT  1655

               END-IF
           END-PERFORM.

       GRAVAR-DADOS.
           MOVE   W-CODIGO   TO   CODIGO.
           MOVE   W-VALOR      TO   VALOR.
           PERFORM     WITH  TEST AFTER UNTIL W-GRAVA  = "S" OR "N"
               ACCEPT  D-GRAVA
               MOVE  FUNCTION  UPPER-CASE (W-GRAVA)  TO  W-GRAVA
               IF   W-GRAVA   =    "S"  OR   "N"
                   DISPLAY      "                 "    AT  1655
               ELSE
                   DISPLAY      "DIGITE   S  OU  N"    AT  1655
               END-IF
           END-PERFORM.
           IF         W-GRAVA   =    "S"
               WRITE       REG-DADOS
           END-IF.


       RECEBER-OPC.
           PERFORM     WITH  TEST   AFTER  UNTIL  W-OPC-OK
               ACCEPT  D-OPC
               MOVE  FUNCTION  UPPER-CASE (W-OPC)  TO  W-OPC
               IF   W-OPC-OK
                   DISPLAY      "                 "    AT  1655
               ELSE
                   DISPLAY      "DIGITE   S  OU  N"    AT  1655
               END-IF
           END-PERFORM.
       ROT-FECHAR.
           CLOSE   ARQ-DADOS.
       FIM.

