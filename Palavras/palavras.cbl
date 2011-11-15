       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             PALAVRAS.
       AUTHOR.                 JULIANO NICOMEDES.
       INSTALLATION.       UNIVERSIDADE PRESBITERIANA MACKENZIE.
       SECURITY.           PROGRAMA DE USO EXCLUSIVO DOS ALUNOS.


       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT IS COMMA.


       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
       77 LINHA-CABEC PIC X(56) VALUE ALL "=".

       01 PALAVRA1-ED.
           02 LETRAP11 PIC X.
           02 LETRAP124 PIC X(3).
       01 PALAVRA2-ED.
           02 LETRAP21 PIC X.
           02 LETRAP22 PIC X.
           02 LETRAP234 PIC X(2).
       01 PALAVRA3-ED.
           02 LETRAP12 PIC X(2).
           02 LETRAP33 PIC X.
           02 LETRAP334 PIC X.
       01 PALAVRA4-ED.
           02 LETRAP413 PIC X(3).
           02 LETRAP44 PIC X.


       77 QQ-TECLA    PIC X     VALUE SPACE.


       PROCEDURE DIVISION.

       INICIO.
           DISPLAY LINHA-CABEC AT 0510.
           DISPLAY "BRINCANDO COM AS PALAVRAS" AT 0625.
           DISPLAY LINHA-CABEC AT 0710.
           DISPLAY "PALAVRA 1:" AT 1030.
           DISPLAY "PALAVRA 2:" AT 1130.
           DISPLAY "PALAVRA 3:" AT 1230.
           DISPLAY "PALAVRA 4:" AT 1330.

           ACCEPT PALAVRA1-ED WITH REQUIRED AT 1042.
           ACCEPT PALAVRA2-ED WITH REQUIRED AT 1142.
           ACCEPT PALAVRA3-ED WITH REQUIRED AT 1242.
           ACCEPT PALAVRA4-ED WITH REQUIRED AT 1342.


           DISPLAY "TECLE <Enter> PARA NOVA PALAVRA" AT  2030.

           ACCEPT QQ-TECLA AT 2070.

           DISPLAY "                               " AT  2030.


           DISPLAY "NOVA PALAVRA:" AT 2530.
           DISPLAY LETRAP11 AT 2545.
           DISPLAY LETRAP22 AT 2546.
           DISPLAY LETRAP33 AT 2547.
           DISPLAY LETRAP44 AT 2548.









       STOP RUN.


