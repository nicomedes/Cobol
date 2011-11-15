       IDENTIFICATION DIVISION.
       PROGRAM-ID.  EXEMPLOFOR.
       AUTHOR.      JULIANO.


       ENVIRONMENT DIVISION.
       CONFIGURATION           SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT IS COMMA.


       DATA DIVISION.

       WORKING-STORAGE SECTION.

       77 IDADE PIC 99 VALUE ZEROS.
      * O NIVEL 88 SERVE PARA NOME DE CONTEUDO OU NIVEL DE CONDICAO
           88 IDADE-OK VALUE 5 THRU 90.

       PROCEDURE DIVISION.

           DISPLAY "DIGITE A IDADE:" AT 1030.


           PERFORM WITH TEST AFTER UNTIL IDADE >= 5 OR IDADE <= 90
               ACCEPT IDADE AT 1046

               IF IDADE >= 5 OR IDADE <= 90
                   DISPLAY "                              " AT 1530
               ELSE
                   DISPLAY "DIGITE UMA IDADE ENTRE 5 E 90!" AT 1530
               END-IF
           END-PERFORM.

           EVALUATE TRUE
               WHEN IDADE >=5 AND < 7 DISPLAY "INFANTIL A"  AT 1130
               WHEN IDADE <= 11 DISPLAY "INFANTIL B"  AT 1130
               WHEN IDADE <= 13 DISPLAY "JUVENIL A"  AT 1130
               WHEN IDADE <= 17 DISPLAY "JUVENIL B"  AT 1130
               WHEN IDADE >=18  DISPLAY "ADULTO"  AT 1130





           END-EVALUATE.





       FIM.


