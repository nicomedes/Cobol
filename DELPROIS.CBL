      $SET ACCEPTREFRESH
       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       DELPROD   INITIAL.
       AUTHOR.           TAKATO.
      *    ************************************************
      *    *  EXEMPLO DE EXCLUSAO RANDOMICA DE ARQUIVO IS *
      *    ************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
           SELECT  CAD-PRODUTO ASSIGN TO "PRODUTO.DAT"
                ORGANIZATION   INDEXED
                RECORD KEY  CODPROD
                ACCESS  RANDOM
                FILE  STATUS  CODERRO.
       DATA DIVISION.
       FILE  SECTION.
       FD  CAD-PRODUTO
           LABEL  RECORD  STANDARD.
       01  REG-PRODUTO.
           02  CODPROD         PIC  9(3).
           02  DESCRI          PIC  X(20).
           02  PRECO           PIC  9(4)V99.
           02  FILLER          PIC  X(41).
       WORKING-STORAGE SECTION.
       77  CODERRO           PIC  X(2)  VALUE SPACES.
       77  OPC               PIC X VALUE SPACE.
       88  OPC-OK            VALUE "S" "N".
       77  OPC-EXCLUI        PIC X VALUE SPACE.
       88  OPC-EXCLUI-OK     VALUE "S" "N".
       77  W-CODPROD-PESQUISA   PIC 9(3) VALUE ZEROS.
       01  DATA-SIS.
           02  ANO  PIC  9999.
           02  MES  PIC  99.
           02  DIA  PIC  99.
       01  DATA-DIA.
           02  DIA  PIC  99/.
           02  MES  PIC  99/.
           02  ANO  PIC  9999.
       01  DATA-COM-BARRA  REDEFINES  DATA-DIA  PIC X(10).

       SCREEN SECTION.
       01  TELA.
           02  BLANK SCREEN BACKGROUND-COLOR 1 FOREGROUND-COLOR 15.
           02  T1 LINE 05 COLUMN 15 VALUE
                                    "EXCLUSAO DO CADASTRO DE PRODUTO".
           02  T11 LINE 07 COLUMN 22 PIC X(10) FROM DATA-COM-BARRA.
           02  T2 LINE 10 COLUMN 10 VALUE "DIGITE PRODUTO A EXCLUIR:".
           02  T3 LINE 12 COLUMN 10 VALUE "CODIGO:".
           02  T4 LINE 14 COLUMN 10 VALUE "DESCRICAO:".
           02  T5 LINE 16 COLUMN 10 VALUE "PRECO:".
           02  T6 LINE 18 COLUMN 10 VALUE
                          "CONFIRMA A EXLUSAO?(S/N): ".
           02  T7 LINE 20 COLUMN 10 VALUE "OUTRO REGISTRO?(S/N):".
       01  TELA-DADOS.
      *
      *    A OPCAO  "USING"    PERMITE MOSTRAR E RECEBER
      *    A OPCAO  "TO"    SO PERMITE RECEBER
      *    A OPCAO  "FROM"  SO PERMITE MOSTRAR
      *
           02  D0 LINE 10 COLUMN 40 PIC ZZ9 TO W-CODPROD-PESQUISA.
           02  D1 LINE 12 COLUMN 22 PIC ZZ9      USING CODPROD.
           02  D2 LINE 14 COLUMN 22 PIC X(20)    USING DESCRI.
           02  D3 LINE 16 COLUMN 22 PIC Z.ZZ9,99 USING PRECO.
           02  D-ALT LINE 18 COLUMN  39 PIC X    TO OPC-EXCLUI AUTO.
           02  D4 LINE 20 COLUMN 35 PIC X        TO    OPC  AUTO.
       PROCEDURE DIVISION.
       INICIO.
           INITIALIZE DATA-SIS.
           ACCEPT  DATA-SIS FROM DATE YYYYMMDD.
           PERFORM ABRIR-ARQUIVO.
           PERFORM PROCESSA UNTIL OPC = "N".
           PERFORM FECHAR-ARQUIVO.
           DISPLAY "FIM DE PROCESSAMENTO" AT 2455.
           STOP RUN.
       PROCESSA.
           INITIALIZE DATA-DIA CODERRO REG-PRODUTO W-CODPROD-PESQUISA.
           MOVE CORR DATA-SIS TO DATA-DIA.
           DISPLAY TELA.

      *PEDE PARA DIGITAR O PRODUTO A SER EXCLUIDO
           ACCEPT D0
           MOVE W-CODPROD-PESQUISA  TO  CODPROD    *> Importante
      *ROTINA PARA LEITURA (PESQUISA) NO CADASTRO DE PRODUTO
           READ CAD-PRODUTO
           IF  CODERRO NOT = "00"
               DISPLAY "PRODUTO NAO FOI ENCONTRADO" AT 1140 WITH BLINK
           ELSE
               PERFORM  EXIBIR-DADOS-DO-REGISTRO
               
               PERFORM  ROTINA-EXCLUIR-REGISTRO
           END-IF.
      *  FIM DA LEITURA EM DISCO
      *
      *  OPCAO PARA CONTINUAR OU PARAR
           PERFORM WITH TEST AFTER UNTIL OPC-OK
               ACCEPT D4
               MOVE FUNCTION UPPER-CASE (OPC) TO OPC
               IF  OPC-OK
                   DISPLAY "                   " AT 2040
               ELSE
                   DISPLAY " DIGITE S OU N" AT 2040
               END-IF
           END-PERFORM.
       EXIBIR-DADOS-DO-REGISTRO.
           DISPLAY  D1
           DISPLAY  D2
           DISPLAY  D3.
       
       ROTINA-EXCLUIR-REGISTRO.
      *  OPCAO PARA EXLUIR OU DESISTIR
           PERFORM WITH TEST AFTER UNTIL OPC-EXCLUI-OK
               ACCEPT D-ALT
               MOVE FUNCTION UPPER-CASE (OPC-EXCLUI) TO OPC-EXCLUI
               IF  OPC-EXCLUI-OK
                   DISPLAY "                   " AT 2040
               ELSE
                   DISPLAY " DIGITE S OU N" AT 2040
               END-IF
           END-PERFORM.
           IF  OPC-EXCLUI  =  "S"
               DELETE  CAD-PRODUTO
           END-IF.


       ABRIR-ARQUIVO.
           OPEN  I-O  CAD-PRODUTO.
           IF  CODERRO NOT = "00"
               DISPLAY "ARQUIVO NAO ENCONTRADO" AT 2040 WITH
                       FOREGROUND-COLOR 4
               STOP  " "
               MOVE  "N"  TO  OPC
           ELSE
               DISPLAY "                       " AT 2040
           END-IF.
       FECHAR-ARQUIVO.
           CLOSE  CAD-PRODUTO.
       FIM-ULTIMA-LINHA.

