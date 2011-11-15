       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG2.
       AUTHOR. JULIANO.


       ENVIRONMENT  DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  N1      PIC 99      VALUE ZERO.
       77  N2      PIC 99      VALUE ZERO.
       77  MED     PIC 99V9    VALUE ZERO.
       77  ED-MED  PIC Z9,9    VALUE ZERO.

       PROCEDURE DIVISION.
       INICIO.

           DISPLAY ERASE.
           DISPLAY "CALCULO DE MEDIA"  AT  0520.
           DISPLAY "NOTA1:"            AT  1010.
           DISPLAY "NOTA2:"            AT  1210.
           DISPLAY "MEDIA:"            AT  1510.
           ACCEPT      N1      AT      1020.
           ACCEPT      N2      AT      1220.
           COMPUTE     MED = (N1+N2)/2.
           MOVE        MED TO ED-MED.
           DISPLAY     ED-MED  AT  1520.
           STOP RUN.



