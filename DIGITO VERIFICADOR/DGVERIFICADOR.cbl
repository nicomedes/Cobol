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
            05 NUM-MATRIC PIC 9(7) VALUE ZEROS.
            05 NUM-AUX PIC 9(7) VALUE ZEROS.
            05 QUOCIENTE PIC 9(7) VALUE ZEROS.
            05 RESTO PIC 99 VALUE ZEROS.
            05 PESO PIC 99 VALUE ZEROS.
            05 SOMA PIC 9(5) VALUE ZEROS.
            05 OPC PIC X VALUE SPACE.
               88 OPC-OK VALUE "S" "N".
      *-----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------*


       INICIO.
           PERFORM ROT-PROCESSA UNTIL OPC = "N".
           DISPLAY ERASE.
           DISPLAY "FIM DO PROCESSAMENTO" AT 1030 WITH BLINK.
       STOP RUN.


       ROT-PROCESSA.
           DISPLAY ERASE.
           INITIALIZE NUM-MATRIC, NUM-AUX, QUOCIENTE, RESTO, PESO, SOMA
           OPC.

           DISPLAY "CALCULO DE DIGITO VERIFICADOR"  AT 0520.
           DISPLAY "NUMERO DE MATRICULA: " AT 1020.
           DISPLAY "DV CALCULADO: "        AT 1120.

           ACCEPT NUM-MATRIC AT 1045.

           MOVE NUM-MATRIC TO NUM-AUX.

           MOVE 2 TO PESO.

           PERFORM WITH TEST AFTER UNTIL QUOCIENTE = 0
               DIVIDE NUM-AUX BY 10 GIVING QUOCIENTE REMAINDER RESTO
               COMPUTE SOMA =  SOMA + (RESTO*PESO)
               ADD 1 TO PESO
               MOVE QUOCIENTE TO NUM-AUX
           END-PERFORM

           DIVIDE SOMA BY 11 GIVING NUM-AUX REMAINDER RESTO.

           IF (RESTO = 0 OR = 1)
               DISPLAY RESTO AT 1135
               ELSE
               COMPUTE RESTO = 11 - RESTO
               DISPLAY RESTO AT 1135
           END-IF.
           DISPLAY "DESEJA FAZER OUTRA VERIFICACAO? (S/N)" AT 2035.

           PERFORM WITH TEST AFTER UNTIL OPC-OK
               ACCEPT OPC WITH AUTO-SKIP UPPER         AT 2080
               IF OPC-OK
                   DISPLAY "                     "      AT 1940
               ELSE
                   DISPLAY "DIGITE N OU S"              AT 1940
               END-IF
           END-PERFORM.





