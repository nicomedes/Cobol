      *-----------------------------------------------------------------*
       IDENTIFICATION  DIVISION.
      *-----------------------------------------------------------------*
       PROGRAM-ID.             IMC.
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

       77 LINHA-CABEC  PIC X(56)     VALUE ALL "=".
       77 NOME-ED      PIC X(10)     VALUE SPACES.
       77 SEXO         PIC X         VALUE SPACES.
           88 SEXO-OK                    VALUE "M" "F".

       77 QQ-TECLA     PIC X(9)      VALUE SPACES.

       77 PESO-ED      PIC ZZ9,99    VALUE ZEROS.
       77 ALTURA-ED    PIC 9,99      VALUE ZEROS.
       77 IMC-ED       PIC ZZ99,9    VALUE ZEROS.
       77 PESOIDEAL-ED PIC ZZ9,99    VALUE ZEROS.

       77 PESO         PIC 999V99    VALUE ZEROS.
           88 PESO-OK                    VALUE 30,0 THRU 200,0.

       77 ALTURA       PIC 9V99      VALUE ZEROS.
           88 ALTURA-OK                  VALUE 0,50 THRU 2,50.

       77 PESOIDEAL    PIC 999V99    VALUE ZEROS.
       77 IMC          PIC 999V99    VALUE ZEROS.
       77 OPC          PIC X         VALUE SPACE.
           88 OPC-OK                 VALUE "S" "N".
      *-----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------*
       INICIO.

           PERFORM PROCESSA UNTIL OPC = "N".
           DISPLAY ERASE.
           DISPLAY "PROGRAMA FINALIZADO!" AT 0618.
           STOP RUN.

        PROCESSA.
           DISPLAY ERASE.
           MOVE ZEROS TO PESO-ED, ALTURA-ED, IMC-ED, PESOIDEAL-ED, PESO,
           ALTURA, PESOIDEAL, IMC.
           MOVE SPACES TO NOME-ED, SEXO, OPC.
           DISPLAY LINHA-CABEC                          AT 0510.
           DISPLAY "ACADEMIA MACK-FIT - CALCULO DE IMC" AT 0618.
           DISPLAY LINHA-CABEC                          AT 0710.
           DISPLAY "NOME..:"                            AT 1018.
           DISPLAY "SEXO..:"                            AT 1118.
           DISPLAY "PESO..:"                            AT 1218.
           DISPLAY "ALTURA:"                            AT 1318.

           PERFORM WITH TEST AFTER UNTIL NOME-ED <> SPACES
               ACCEPT NOME-ED WITH UPPER                AT 1030
               IF NOME-ED <> SPACES
                   DISPLAY "                     "      AT 1940
               ELSE
                   DISPLAY "DIGITE UM NOME!      "      AT 1940
               END-IF
           END-PERFORM.

           PERFORM WITH TEST AFTER UNTIL SEXO-OK
               ACCEPT SEXO WITH AUTO-SKIP UPPER         AT 1130
               IF SEXO-OK
                   DISPLAY "                     "      AT 1940
               ELSE
                   DISPLAY "DIGITE M OU F"              AT 1940
               END-IF
           END-PERFORM.

           PERFORM WITH TEST AFTER UNTIL PESO-OK
               ACCEPT PESO-ED                          AT 1230
               MOVE PESO-ED TO PESO
               IF PESO-OK
                   DISPLAY "                               " AT 1940
               ELSE
                   DISPLAY "DIGITE UM PESO ENTRE 30,0 E 200" AT 1940
               END-IF
           END-PERFORM.
           PERFORM WITH TEST AFTER UNTIL ALTURA-OK
               ACCEPT ALTURA-ED                         AT 1330
               MOVE ALTURA-ED TO ALTURA
               IF ALTURA-OK
                   DISPLAY "                                   " AT 1940
               ELSE
                   DISPLAY "DIGITE UMA ALTURA ENTRE 0,50 E 2,50" AT 1940
               END-IF
           END-PERFORM.

           DISPLAY "Tecle <Enter> para resultados"      AT 2040.
           ACCEPT QQ-TECLA WITH AUTO-SKIP               AT 2140.

           MOVE PESO-ED TO PESO.
           MOVE ALTURA-ED TO ALTURA.
           COMPUTE IMC = PESO / (ALTURA**2).
           MOVE IMC TO IMC-ED.

           IF SEXO = "M"
               COMPUTE PESOIDEAL = 72,7 * ALTURA - 58
           ELSE
               IF SEXO = "F"
               COMPUTE PESOIDEAL = 62,1 * ALTURA - 44,7
               END-IF
           END-IF.

           MOVE PESOIDEAL TO PESOIDEAL-ED.

           DISPLAY ERASE.
           DISPLAY LINHA-CABEC                          AT 0510.
           DISPLAY "ACADEMIA MACK-FIT - RESULTADOS"     AT 0618.
           DISPLAY LINHA-CABEC                          AT 0710.
           DISPLAY "NOME..........:"                    AT 1018.
           DISPLAY NOME-ED                              AT 1035.
           DISPLAY "SEU PESO IDEAL:"                    AT 1118.
           DISPLAY PESOIDEAL-ED                         AT 1135.
           DISPLAY "SEU IMC.......:"                    AT 1218.
           DISPLAY IMC-ED                               AT 1235.

           IF IMC <= 18,5
               DISPLAY "ABAIXO DO PESO!" AT 1242
           ELSE
               IF IMC > 18,5 AND < 24,9
               DISPLAY "PESO NORMAL" AT 1242
               ELSE
                   IF IMC > 25 AND < 29,9
                   DISPLAY "ACIMA DO PESO" AT 1242
                   ELSE
                       IF IMC > 30
                       DISPLAY "OBESO!"    AT 1242
                       END-IF
                   END-IF
               END-IF
            END-IF
           DISPLAY "OUTRO CALCULO? (S/N): "               AT 2040.
           PERFORM WITH TEST AFTER UNTIL OPC-OK

               ACCEPT OPC WITH AUTO-SKIP UPPER                AT 2062
               IF OPC-OK
                   DISPLAY "                                   " AT 1940
               ELSE
                   DISPLAY "DIGITE S OU N"                       AT 1940
               END-IF
           END-PERFORM.
