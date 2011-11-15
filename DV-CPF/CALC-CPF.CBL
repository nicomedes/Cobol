      $SET  ACCEPTREFRESH
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.            CALC-CPF.
       AUTHOR.          TAKATO.
       ENVIRONMENT      DIVISION.
       DATA         DIVISION.
       WORKING-STORAGE    SECTION.
       01    VARIAVEIS.
           05    NUMERO            PIC   9(9)  VALUE  ZEROS.
           05    NUMERO-AUX        PIC   9(9)  VALUE  ZEROS.
           05    PESO              PIC   99   VALUE  2.
           05    SOMA              PIC   9(3)  VALUE   ZEROS.
           05    QUOCIENTE         PIC   9(9)   VALUE  ZEROS.
           05    RESTO             PIC   99      VALUE  ZEROS.
           05        OPC           PIC   X    VALUE   SPACE.            .
               88    OPC-OK            VALUE  "S"  "N".
           05    DC.
               10    DC1    PIC   9    VALUE  0.
               10    DC2    PIC   9    VALUE  0.
           05    LIMPA-MSG        PIC  X(30)  VALUE  SPACES.
       SCREEN   SECTION.
       01    TELA1.
           05    BLANK   SCREEN.
           05    LINE  05   COLUMN  20  VALUE
                               "CALCULO DE DC DO CPF".
           05    LINE  10   COLUMN  10   VALUE
                 "CPF: ".
           05    LINE  14   COLUMN  10   VALUE
                 "DC CALCULADO:  ".
           05    LINE  20   COLUMN  10   VALUE
                 "OUTRO CALCULO?(S/N):  ".
       01    TELA2.
           05   L-CPF    LINE  10  COLUMN  25  PIC  9(9)  TO  NUMERO
                                       REQUIRED   AUTO.
           05   L-DC      LINE   14  COLUMN  25  PIC  X(2)  FROM  DC.
           05   L-OPC    LINE  20  COLUMN  40  PIC  X  TO  OPC  AUTO.

       PROCEDURE   DIVISION.
       INICIO.
           PERFORM  ROT-PROCESSA  UNTIL  OPC  =  "N".
           STOP   RUN.
       ROT-PROCESSA.
           PERFORM   FORMATAR-TELA.
           PERFORM   RECEBER-CPF.
           PERFORM   CALCULAR-DC.
           PERFORM   EXIBIR-DC.
           PERFORM   RECEBER-OPC.
       FIM-ROT-PROCESSA.     EXIT.
       FORMATAR-TELA.
           DISPLAY  TELA1.
       FIM-FORMATAR-TELA.   EXIT.
       RECEBER-CPF.
           INITIALIZE   VARIAVEIS.
           ACCEPT L-CPF.
       FIM-RECEBER-CPF.    EXIT.
       CALCULAR-DC.
      *    *****  CALCULO DE DC1  *****
           MOVE   2   TO   PESO.
           PERFORM   ROT-AUX-CALC.
           IF      RESTO  <  2
               MOVE   0  TO   DC1
           ELSE
               COMPUTE   DC1  =  11  -   RESTO
           END-IF.
      *    *****  CALCULO DE DC2  *****
           MOVE   ZEROS   TO  SOMA.
           COMPUTE   SOMA  =   SOMA  +  DC1  *  2.
           MOVE   3   TO  PESO.
           PERFORM   ROT-AUX-CALC.
           IF    RESTO  <   2
               MOVE  0   TO  DC2
           ELSE
               COMPUTE   DC2  =  11  -  RESTO
           END-IF.
       FIM-CALCULAR-DC.     EXIT.
       ROT-AUX-CALC.
           MOVE  NUMERO  TO  NUMERO-AUX
           PERFORM  WITH  TEST  AFTER  UNTIL  QUOCIENTE  =  0
               DIVIDE   NUMERO-AUX  BY  10
                         GIVING   QUOCIENTE  REMAINDER   RESTO
               MOVE  QUOCIENTE   TO  NUMERO-AUX
               COMPUTE   SOMA  =   SOMA  +  RESTO  *  PESO
               ADD   1   TO   PESO
           END-PERFORM.
           DIVIDE  SOMA  BY  11  GIVING  QUOCIENTE  REMAINDER  RESTO.
       FIM-ROT-AUX-CALC.   EXIT.
       EXIBIR-DC.
           DISPLAY   L-DC.
       FIM-EXIBIR-DC.    EXIT.
       RECEBER-OPC.
           PERFORM WITH TEST AFTER UNTIL OPC-OK
               ACCEPT  L-OPC
               MOVE  FUNCTION  UPPER-CASE  (OPC)  TO  OPC
               IF  OPC-OK
                   DISPLAY   LIMPA-MSG  AT  2050
               ELSE
                   DISPLAY  "DIGITE S OU N"  AT  2050
               END-IF
           END-PERFORM.
       FIM-DO-PROGRAMA.    EXIT.
