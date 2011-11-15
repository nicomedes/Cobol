      *-----------------------------------------------------------------*
       IDENTIFICATION          DIVISION.
      *-----------------------------------------------------------------*

       PROGRAM-ID.             DGVERIFICADOR.
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
       01 VARIAVEIS.
            05 NUM-AUX PIC 9(7) VALUE ZEROS.
            05 QUOCIENTE PIC 9(7) VALUE ZEROS.
            05 RESTO PIC 99 VALUE ZEROS.
            05 PESO PIC 99 VALUE ZEROS.
            05 SOMA PIC 9(5) VALUE ZEROS.
            05 OPC PIC X VALUE SPACE.
               88 OPC-OK VALUE "S" "N".
            05 DV-CALC PIC 99 VALUE ZEROS.
       77 BRANCO PIC X(7) VALUE SPACES.

       SCREEN SECTION.
       01 TELA.
         02 BLANK SCREEN.
         02 L1 LINE 05 COLUMN 10 VALUE "CALCULO DE DIGITO VERIFICADOR".
         02 L2 LINE 10 COLUMN 10 VALUE "DIGITE NUMERO DE MATRICULA".
         02 L21 LINE 10 COLUMN 45 VALUE "(7 DIGITOS)".
         02 L3 LINE 14 COLUMN 10 VALUE "DV CALCULADO:".
         02 L4 LINE 20 COLUMN 10 VALUE "OUTRO CALCULO? (S/N):".
         02 L5 LINE 10 COLUMN 37 PIC 9(3).9(4) FROM NUM-AUX.


       01 TELA-DADOS.
          02 D1 LINE 10 COLUMN 37 PIC 9(3).9(4) TO NUM-AUX.
          02 D3 LINE 20 COLUMN 45 PIC X TO OPC AUTO-SKIP.
          02 D2 LINE 14 COLUMN 30 PIC 99 FROM DV-CALC.
       01  LIMPA.
           02 LINE 14 COLUMN 30 PIC X(7) FROM BRANCO.

      *-----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------*

       INICIO.
           PERFORM ROT-PROCESSA UNTIL OPC = "N".
           DISPLAY "FIM DO PROCESSAMENTO" AT 1030 WITH BLINK.
       STOP RUN.

       ROT-PROCESSA.
           DISPLAY TELA.
           DISPLAY LIMPA.

           INITIALIZE VARIAVEIS.

           ACCEPT D1
           MOVE 2 TO PESO.

           PERFORM WITH TEST AFTER UNTIL QUOCIENTE = 0
               DIVIDE NUM-AUX BY 10 GIVING QUOCIENTE REMAINDER RESTO
               COMPUTE SOMA =  SOMA + (RESTO*PESO)
               ADD 1 TO PESO
               MOVE QUOCIENTE TO NUM-AUX
           END-PERFORM

           DIVIDE SOMA BY 11 GIVING NUM-AUX REMAINDER RESTO.

           IF (RESTO = 0 OR = 1)
               MOVE RESTO TO DV-CALC
               ELSE
               COMPUTE RESTO = 11 - RESTO
               MOVE RESTO TO DV-CALC
           END-IF.

           DISPLAY D2.

           PERFORM WITH TEST AFTER UNTIL OPC-OK
               ACCEPT D3
               IF OPC-OK
                   DISPLAY "                     "      AT 1940
               ELSE
                   DISPLAY "DIGITE N OU S"              AT 1940
               END-IF
           END-PERFORM.
