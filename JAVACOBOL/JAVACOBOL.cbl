      *-----------------------------------------------------------------*
       IDENTIFICATION  DIVISION.
      *-----------------------------------------------------------------*

       PROGRAM-ID. JAVACOBOL.
       AUTHOR. JULIANO NICOMEDES.
       INSTAlLATION. UNIVERSIDADE PRESBITERIANA MACKENZIE.
       SECURITY. PROGRAMA DE USO EXCLUSIVO DOS ALUNOS.

      *-----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------*

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------*
       DATA                    DIVISION.
      *-----------------------------------------------------------------*

       WORKING-STORAGE         SECTION.
       01 VARIAVEIS.
           02 NUMERO-REC PIC -ZZZ,99  VALUE ZEROS.
           02 NUMERO     PIC 999V99 VALUE ZEROS.
           02 OPC        PIC 9  VALUE ZEROS.
               88 OPC-OK           VALUE 1 THRU 6.
           02 SOMA       PIC 999V99 VALUE ZEROS.
           02 SOMAP      PIC 999V99 VALUE ZEROS.
           02 SOMAIP     PIC 999V99 VALUE ZEROS.
           02 MAIOR      PIC 999V99 VALUE ZEROS.
           02 MENOR      PIC 999V99 VALUE ZEROS.
           02 DIFERENCA  PIC 999V99 VALUE ZEROS.
           02 MEDIA      PIC 999V99 VALUE ZEROS.
           02 CONT       PIC 999   VALUE ZEROS.
           02 CONTI      PIC 999 VALUE ZEROS.
           02 RESTO      PIC 999V99 VALUE ZEROS.
           02 DIVRESULT  PIC 999V99 VALUE ZEROS.
           02 RESULTADO-ED  PIC ZZ9,99 VALUE ZEROS.

      *-----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------*

       INICIO.
           MOVE ZEROS TO VARIAVEIS.
           DISPLAY "DIGITE VARIOS NUMEROS:" AT 0110.
           DISPLAY "(-1 ENCERRA)" AT 0150.
           PERFORM WITH TEST AFTER UNTIL NUMERO = -1
               ACCEPT NUMERO-REC AT 0135
               MOVE NUMERO-REC TO NUMERO

               MOVE ZERO TO NUMERO-REC

               IF NUMERO NOT = -1
                   PERFORM ROT-CALCULO
               END-IF
           END-PERFORM.

           ROT-MENU.

               DISPLAY "1 - SOMA DE NUMEROS" AT 1010.
               DISPLAY "2 - MEDIA ARITMETICA DOS NUMEROS" AT 1110.
               DISPLAY "3 - QUANTIDADE DE IMPARES" AT 1210.
               DISPLAY "4 - SOMA DOS PARES" AT 1310.
               DISPLAY "5 - DIFERENCA ENTRE O MAIOR E MENOR" AT 1410.
               DISPLAY "6 - SAIR" AT 1510.

               PERFORM WITH TEST AFTER UNTIL OPC-OK

               DISPLAY "                                    "AT 1620

               DISPLAY "ESCOLHA UMA OPCAO ( )"   AT 1620

               ACCEPT OPC AT 1639 WITH AUTO-SKIP

               IF OPC-OK
                   DISPLAY "                                    "AT 1620

                   EVALUATE OPC
                       WHEN 1
                           DISPLAY "SOMA:                       "AT 2010
                           MOVE SOMA TO RESULTADO-ED
                           DISPLAY RESULTADO-ED    AT 2017
                       WHEN 2
                           DISPLAY "MEDIA:                      "AT 2010
                           MOVE MEDIA TO RESULTADO-ED
                           DISPLAY RESULTADO-ED  AT 2018
                       WHEN 3
                           DISPLAY "QUANTIDADE DE IMPARES:      "AT 2010
                           MOVE CONTI TO RESULTADO-ED
                           DISPLAY RESULTADO-ED AT   2033
                       WHEN 4
                           DISPLAY "SOMA DE PARES:              "AT 2010
                           MOVE SOMAP TO RESULTADO-ED
                           DISPLAY RESULTADO-ED AT 2030
                       WHEN 5
                           DISPLAY "DIFERENCA MAIOR/MENOR:      "AT 2010
                           MOVE DIFERENCA TO RESULTADO-ED
                           DISPLAY RESULTADO-ED AT 2034
                       WHEN 6
                           DISPLAY "PROGRAMA FINALIZADO!        "AT 2010
                           STOP RUN
      *----------NAO USEI WHEN OTHER POIS FIZ A VALIDACAO ANTES
                   END-EVALUATE

                   PERFORM ROT-MENU

               ELSE
                   DISPLAY "ESCOLHA UMA OPCAO ENTRE 1 E 6!"
                   AT 1620

               END-IF

               END-PERFORM.



           END-ROT-MENU.



           ROT-CALCULO.

               ADD NUMERO TO SOMA.
               ADD 1 TO CONT.
               DIVIDE NUMERO BY 2 GIVING DIVRESULT REMAINDER RESTO
               IF (RESTO = 0)
                   COMPUTE SOMAP = SOMAP + NUMERO
               ELSE
                  ADD 1 TO CONTI
               END-IF.

               IF NUMERO > MAIOR
                   MOVE NUMERO TO MAIOR
               ELSE
                   IF NUMERO < MENOR
                   MOVE NUMERO TO MENOR
                   END-IF
               END-IF.
               COMPUTE MEDIA = SOMA / CONT.
               COMPUTE DIFERENCA = MAIOR - MENOR.

            FIM-ROT-CALCULO.
       FIM.
