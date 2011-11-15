      *-----------------------------------------------------------------*
       IDENTIFICATION          DIVISION.
      *-----------------------------------------------------------------*

       PROGRAM-ID.             SEGURDOFA.
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
           02 NOME PIC X(15) VALUE SPACES.
           02 IDADE PIC 9(2) VALUE ZEROS.
           02 GRUPO PIC X VALUE SPACES.
           02 CATEGORIA PIC X(20) VALUE SPACES.


      *-----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------*

       INICIO.
           INITIALIZE VARIAVEIS.
           ACCEPT NOME.
           ACCEPT IDADE.
           ACCEPT GRUPO.

            EVALUATE TRUE
               WHEN (GRUPO = "B")
                   EVALUATE TRUE
                       WHEN IDADE > 17 AND <= 20
                           MOVE " CATEGORIA 1" TO CATEGORIA
                       WHEN IDADE > 21 AND <= 24
                           MOVE " CATEGORIA 2" TO CATEGORIA
                       WHEN IDADE > 25 AND <= 34
                           MOVE " CATEGORIA 3" TO CATEGORIA
                       WHEN IDADE > 35 AND <= 64
                           MOVE " CATEGORIA 4" TO CATEGORIA
                       WHEN IDADE > 65 AND <= 70
                           MOVE " CATEGORIA 7" TO CATEGORIA
                   END-EVALUATE
                WHEN (GRUPO = "M")
                   EVALUATE TRUE
                       WHEN IDADE > 17 AND <= 20
                           MOVE " CATEGORIA 2" TO CATEGORIA
                       WHEN IDADE > 21 AND <= 24
                           MOVE " CATEGORIA 3" TO CATEGORIA
                       WHEN IDADE > 25 AND <= 34
                           MOVE " CATEGORIA 4" TO CATEGORIA
                       WHEN IDADE > 35 AND <= 64
                           MOVE " CATEGORIA 5" TO CATEGORIA
                       WHEN IDADE > 65 AND <= 70
                           MOVE " CATEGORIA 8" TO CATEGORIA
                   END-EVALUATE

                WHEN (GRUPO = "A")
                   EVALUATE TRUE
                       WHEN IDADE > 17 AND <= 20
                           MOVE " CATEGORIA 3" TO CATEGORIA
                       WHEN IDADE > 21 AND <= 24
                           MOVE " CATEGORIA 4" TO CATEGORIA
                       WHEN IDADE > 25 AND <= 34
                           MOVE " CATEGORIA 5" TO CATEGORIA
                       WHEN IDADE > 35 AND <= 64
                           MOVE " CATEGORIA 6" TO CATEGORIA
                       WHEN IDADE > 65 AND <= 70
                           MOVE " CATEGORIA 9" TO CATEGORIA
                   END-EVALUATE



            END-EVALUATE.

            DISPLAY NOME ", " IDADE  CATEGORIA.




       STOP-RUN.




