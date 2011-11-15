       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MEDIA.
       AUTHOR.      JULIANO.


       ENVIRONMENT DIVISION.
       CONFIGURATION           SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT IS COMMA.


       DATA DIVISION.

       WORKING-STORAGE SECTION.
      *    SEMPRE INICIALIZAR AS VARIAVIES. NESSE CASO COM 0.
      *    COLOCAR -ED PARA IDENTIFICAR VARIAVEIS DE EDICAO.
       77    NOME     PIC X(30) VALUE SPACES.
       77    NOTA1-ED PIC Z9,9 VALUE ZEROS.
       77    NOTA2-ED PIC Z9,9 VALUE ZEROS.
       77    NOTA3-ED PIC Z9,9 VALUE ZEROS.
       77    NOTA4-ED PIC Z9,9 VALUE ZEROS.
       77    MEDIA-ED PIC Z9,9 VALUE ZEROS.

       77    NOTA1 PIC 99V9 VALUE ZEROS.
       77    NOTA2 PIC 99V9 VALUE ZEROS.
       77    NOTA3 PIC 99V9 VALUE ZEROS.
       77    NOTA4 PIC 99V9 VALUE ZEROS.



       77    SOMA PIC  99V99 VALUE ZEROS.
       77    MEDIA PIC 99V9 VALUE ZEROS.

       PROCEDURE DIVISION.

       INICIO.
           DISPLAY "NOME:  " AT 1030.
           DISPLAY "NOTA1: " AT 1130.
           DISPLAY "NOTA2: " AT 1230.
           DISPLAY "NOTA3: " AT 1330.
           DISPLAY "NOTA4: " AT 1430.

           ACCEPT NOME       AT 1036.
           ACCEPT NOTA1-ED   AT 1136.
           ACCEPT NOTA2-ED   AT 1236.
           ACCEPT NOTA3-ED   AT 1336.
           ACCEPT NOTA4-ED   AT 1436.

           MOVE NOTA1-ED TO NOTA1.
           MOVE NOTA2-ED TO NOTA2.
           MOVE NOTA3-ED TO NOTA3.
           MOVE NOTA4-ED TO NOTA4.

           COMPUTE SOMA = (NOTA1+NOTA2+NOTA3+NOTA4).
           COMPUTE MEDIA = SOMA/4.

           MOVE MEDIA TO MEDIA-ED.

           DISPLAY ERASE.



           IF MEDIA < 7
               DISPLAY NOME        AT 0930
               DISPLAY "VOCE FOI REPROVADO COM " AT 1030
               DISPLAY MEDIA-ED                  AT 1052
           ELSE
               DISPLAY NOME        AT 0930
               DISPLAY "VOCE FOI APROVADO COM " AT 1030
               DISPLAY MEDIA-ED                 AT 1052
           END-IF.















       FIM.


