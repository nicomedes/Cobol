      *-----------------------------------------------------------------*
       IDENTIFICATION          DIVISION.
      *-----------------------------------------------------------------*

       PROGRAM-ID.             PEOPLE.
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

       01 WS-PERSON-RECORD-TABLE.
           03 WS-EMPLOYEES OCCURS 10 TIMES.
               05 WS-NAME PIC X(30).
               05 WS-SEX  PIC X.
       01 WS-COUNT        PIC 99 VALUE 0.
       01 WS-COUNTM       PIC 99 VALUE ZEROS.
       01 WS-COUNTF       PIC 99 VALUE ZEROS.
      *-----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------*

       INICIO.
           INITIALIZE WS-PERSON-RECORD-TABLE.
           PERFORM UNTIL WS-COUNT >= 3
               ADD 1 TO WS-COUNT
               ACCEPT WS-NAME(WS-COUNT)
               ACCEPT WS-SEX(WS-COUNT)
                   IF WS-SEX(WS-COUNT) = "H" OR "h"
                       ADD 1 TO WS-COUNTM
                   END-IF
                   IF WS-SEX(WS-COUNT) = "M" OR "m"
                       ADD 1 TO WS-COUNTF
                   END-IF
           END-PERFORM.
           DISPLAY "QUANTIDADE DE PESSOAS "  WS-COUNT.
           DISPLAY "QUANTIDADE DE HOMENS "   WS-COUNTM.
           DISPLAY "QUANTIDADE DE MULHERES " WS-COUNTF.
       STOP-RUN.


