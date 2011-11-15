       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         TESTEPIC.
       AUTHOR.             TAKATO.
       DATE-WRITTEN.        23/08/2011.
       DATE-COMPILED.
       INSTALLATION.       UNIVERSIDADE PRESBITERIANA MACKENZIE.
       SECURITY.           PROGRAMA DE USO EXCLUSIVO DOS ALUNOS.

       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                DIVISION.
       WORKING-STORAGE     SECTION.

       77 W-EMISSOR        PIC X(6)    VALUE  "ABCDE5"   .
       77 W-RECEPTOR       PIC XXBXXXBBX.


       PROCEDURE           DIVISION.
       INICIO.
           DISPLAY     ERASE.
           MOVE W-EMISSOR TO W-RECEPTOR.
           DISPLAY "CONTEUDO EDITADO = "   AT 1215.
           DISPLAY W-RECEPTOR              AT 1240.

           STOP RUN.

