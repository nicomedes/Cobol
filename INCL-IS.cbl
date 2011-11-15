      $SET ACCEPTREFRESH
       IDENTIFICATION DIVISION.
       PROGRAM-ID.             CRIAPROD.
       AUTHOR.                 TAKATO.
      *     ****************************************************
      *     *    PROGRAMA DE INCLUSAO NO CADASTRO DE PRODUTOS  *
      *     *       VERIFICA A DUPLICIDADE DE REGISTROS        *
      *     ****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-PROD ASSIGN TO "PRODUTO.DAT"
               ORGANIZATION INDEXED
               RECORD KEY COD-PROD
               ACCESS RANDOM
               FILE STATUS IS W-COD-ERRO.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-PROD
           LABEL RECORD STANDARD.
       01  REG-PROD.
           02  COD-PROD    PIC 9(3).
           02  DESCRI-PROD PIC X(20).
           02  PRECO-PROD  PIC 9(4)V99.
           02  FILLER      PIC X(41).
       WORKING-STORAGE SECTION.
       01  W-REG-PROD.
           02  W-COD-PROD    PIC 9(3).
           02  W-DESCRI-PROD PIC X(20).
           02  W-PRECO-PROD  PIC 9(4)V99.
           02  W-FILLER      PIC X(41).
       01  W-COD-ERRO          PIC XX VALUE SPACES.
       01  W-OPCAO             PIC X  VALUE SPACE.
       01  W-INCLUI          PIC X  VALUE SPACE.
       01  W-BRANCO          PIC X(50) VALUE SPACE.
       SCREEN  SECTION.
       01  TELA1.
           02  BLANK SCREEN.
           02  LINE 05 COLUMN 20 VALUE "INCLUSAO DE PRODUTOS".
           02  LINE 10 COLUMN 10 VALUE "CODIGO:".
           02  LINE 12 COLUMN 10 VALUE "DESCRICAO:".
           02  LINE 14 COLUMN 10 VALUE "PRECO UNIT.:".
           02  LINE 18 COLUMN 10 VALUE "CONFIRMA A INCLUSAO?(S/N):".
           02  LINE 24 COLUMN 10 VALUE "MENSAGEM:".
       01  TELADADOS.
           02  D-COD LINE 10 COLUMN 25 PIC ZZ9 TO W-COD-PROD.
           02  D-DESCRI LINE 12 COLUMN 25 PIC X(20) TO W-DESCRI-PROD.
           02  D-PRECO LINE 14 COLUMN 25 PIC Z.ZZ9,99 TO W-PRECO-PROD.
       PROCEDURE DIVISION.
       INICIO.
           PERFORM INICIALIZACAO.
           PERFORM PROCESSAMENTO UNTIL W-OPCAO = "N".
           PERFORM FINALIZACAO.
           STOP RUN.
       INICIALIZACAO.
           PERFORM   LIMPAR-VARIAVEIS.
           OPEN  I-O  ARQ-PROD.
      *
      *    Se  W-COD-ERRO   retornar codigo 05 significa que
      *    o arquivo nao existe e portanto esta criando um novo arquivo
      *
       PROCESSAMENTO.
           PERFORM FORMATAR-TELA.
           PERFORM RECEBER-DADOS.
           PERFORM GRAVAR-DADOS.
           PERFORM OPCAO-CONTINUIDADE.
       FORMATAR-TELA.
           DISPLAY  TELA1.
       RECEBER-DADOS.
           PERFORM  WITH TEST AFTER UNTIL
                    W-COD-PROD > 100 AND < 500
               ACCEPT D-COD
               IF  W-COD-PROD <= 100 OR >= 500
                   DISPLAY "CODIGO DEVERA SER > 100 E < 500"
                           AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.
           PERFORM WITH TEST AFTER UNTIL
                   W-DESCRI-PROD NOT = SPACES
               ACCEPT D-DESCRI
               IF  W-DESCRI-PROD = SPACES
                   DISPLAY "DESCRICAO - CAMPO OBRIGATORIO    " AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.
           PERFORM WITH TEST AFTER UNTIL
                   W-PRECO-PROD > 0 AND <= 10000,00
               ACCEPT D-PRECO
               IF  W-PRECO-PROD = 0 OR > 10000,00
                   DISPLAY "PRECO UNIT. DEVERA SER > 0 E <= 10.000,00"
                           AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.
       GRAVAR-DADOS.
           PERFORM WITH TEST AFTER UNTIL
                   W-INCLUI = "S" OR "N"
               ACCEPT W-INCLUI AT  1845 WITH UPPER
               IF  W-INCLUI NOT = "S" AND "N"
                   DISPLAY "DIGITAR S PARA GRAVAR E N PARA DESITIR"
                           AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.
           IF  W-INCLUI = "S"
               WRITE  REG-PROD  FROM  W-REG-PROD
               IF W-COD-ERRO NOT = "00"
                  DISPLAY "REGISTRO DUPLICADO" AT 2421  WITH
                          FOREGROUND-COLOR 4
               ELSE
                  DISPLAY "                   " AT 2421
               END-IF
           ELSE
               PERFORM 7000 TIMES
               DISPLAY "REGISTRO SENDO DESCARTADO ...  " AT 2421
               END-PERFORM
               DISPLAY W-BRANCO AT 2421
           END-IF.
       OPCAO-CONTINUIDADE.
           DISPLAY "DESEJA INCLUIR OUTRO REGISTRO?(S/N):" AT 2220
           PERFORM WITH TEST AFTER UNTIL
                   W-OPCAO = "S" OR "N"
               ACCEPT W-OPCAO AT  2265 WITH UPPER
               IF  W-OPCAO NOT = "S" AND "N"
                   DISPLAY "DIGITAR S PARA INCLUIR OUTRO REGISTRO E N PA
      -                    "RA TERMINAR"  AT 2421
               ELSE
                   DISPLAY W-BRANCO AT 2421
               END-IF
           END-PERFORM.
       LIMPAR-VARIAVEIS.
           MOVE ZEROS TO W-COD-PROD W-PRECO-PROD.
           MOVE SPACES TO W-DESCRI-PROD  W-INCLUI W-OPCAO.
       FINALIZACAO.
           CLOSE  ARQ-PROD.
           DISPLAY "TERMINO DO PROCESSAMENTO" AT 2421.
       FIM.






