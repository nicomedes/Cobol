      $SET ACCEPTREFRESH
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  DV-NMAT.
       AUTHOR.       TAKATO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NUMERO          PIC 9(7)  VALUE ZEROS.
       01  RESTO           PIC 99    VALUE ZEROS.
       01  PESO            PIC 99    VALUE ZEROS.
       01  SOMA            PIC 9(3)  VALUE ZEROS.
       01  DIGITO          PIC 9     VALUE ZEROS.
       01  OPCAO           PIC X     VALUE SPACE.

       SCREEN SECTION.
       01  TELA.
           02  BLANK SCREEN.
           02  LINE 05 COLUMN 10 VALUE
                   "CALCULO DE DIGITO VERIFICADOR".
           02  LINE 10 COLUMN 10 VALUE
                   "DIGITE NUMERO DE MATRICULA (SEM DV):".
           02  NUMERO-TELA
               LINE 10 COLUMN 50 PIC 9.99.9999 TO NUMERO.
           02  LINE 14 COLUMN 10 VALUE
                   "DIGITO VERIFICADOR CALCULADO: ".
           02  DIGITO-TELA
               LINE 14 COLUMN 40 PIC 9 FROM DIGITO
               FOREGROUND-COLOR 3.
           02  LINE 20 COLUMN 10 VALUE
                   "OUTRO CALCULO?(S/N): ".

       PROCEDURE DIVISION.
       INICIO.
           PERFORM PROCESSA UNTIL OPCAO = "N".
           STOP RUN.
       PROCESSA.
           PERFORM LIMPAR-VARIAVEIS.
           PERFORM FORMATAR-TELA.
           PERFORM RECEBER-DADO.
           CALL "MODULO-CALC-DV" USING NUMERO DIGITO.
           PERFORM EXIBIR-DV.
           PERFORM RECEBER-OPCAO.
       FIM-PROCESSA.  EXIT.
       LIMPAR-VARIAVEIS.
           MOVE ZEROS TO NUMERO SOMA RESTO DIGITO PESO.
       FORMATAR-TELA.
           DISPLAY TELA.
       RECEBER-DADO.
           ACCEPT TELA.
       EXIBIR-DV.
           DISPLAY  DIGITO-TELA.
       RECEBER-OPCAO.
           MOVE SPACE TO OPCAO.
           PERFORM WITH TEST AFTER UNTIL OPCAO = "S" OR "N"
               ACCEPT   OPCAO  AT 2032 WITH UPPER
               IF  OPCAO NOT = "S" AND "N"
                   DISPLAY "DIGITE S OU N" AT 2040
               ELSE
                   DISPLAY "              " AT 2040
               END-IF.
       FIM.
