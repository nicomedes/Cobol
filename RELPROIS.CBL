       IDENTIFICATION DIVISION.
       PROGRAM-ID.             RELPROD.
       AUTHOR.                 TAKATO-SOLANGE.
      *     ****************************************************
      *     *              PROGRAMA DE RELATORIO               *
      *     *       IMPRIME CONTEUDO DE CADASTRO DE PRODUTO    *
      *     ****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-PROD ASSIGN TO "PRODUTO.DAT"
               ORGANIZATION INDEXED
               RECORD KEY  COD-PROD
	       ACCESS   SEQUENTIAL
               FILE STATUS IS W-COD-ERRO.
           SELECT ARQ-IMP  ASSIGN TO "RELAT.PRN".
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-PROD
           LABEL RECORD STANDARD.
       01  REG-PROD.
           02  COD-PROD    PIC 9(3).
           02  DESCRI-PROD PIC X(20).
           02  PRECO-PROD  PIC 9(4)V99.
           02  FILLER      PIC X(41).
       FD  ARQ-IMP
           LABEL  RECORD  OMITTED.
       01  REG-IMP         PIC X(80).
       WORKING-STORAGE SECTION.
       01  CABEC1.
           02  FILLER1     PIC X(09) VALUE SPACES.
           02  FILLER1     PIC X(51) VALUE
           "LISTAGEM DE CADASTRO DE PRODUTO".
           02  FILLER1     PIC X(5)  VALUE "PAG.".
           02  PAG-CAB     PIC Z9.
       01  CABEC2.
           02  FILLER1     PIC X(19) VALUE "CODIGO".
           02  FILLER1     PIC X(35) VALUE "DESCRICAO".
           02  FILLER1     PIC X(5)  VALUE "PRECO".
       01  DETALHE.
           02  COD-DET     PIC ZZ9.
           02  FILLER1     PIC X(16) VALUE SPACES.
           02  DESCRI-DET  PIC X(20).
           02  FILLER1     PIC X(12) VALUE SPACES.
           02  PRECO-DET   PIC Z.ZZ9,99.
       01  MEDIA-TOT.
           02  FILLER1     PIC X(51) VALUE "PRECO MEDIO".
           02  MEDIA-DET   PIC Z.ZZ9,99.

       01  W-COD-ERRO          PIC XX VALUE SPACES.
       01  W-CONT-LINHA        PIC 99  VALUE 50.
       01  W-CONT-PAGINA       PIC 99  VALUE ZEROS.
       01  W-CONT-REGISTRO     PIC 9(5) VALUE ZEROS.
       01  W-ACUM-PRECO        PIC 9(9)V99  VALUE ZEROS.
       01  W-MEDIA             PIC 9(5)V99 VALUE ZEROS.
       PROCEDURE DIVISION.
       INICIO.
           PERFORM INICIALIZACAO.
           PERFORM PROCESSAMENTO UNTIL W-COD-ERRO = "10".
           PERFORM FINALIZACAO.
           STOP RUN.
       INICIALIZACAO.
           OPEN  INPUT  ARQ-PROD
                 OUTPUT ARQ-IMP.
           PERFORM LEITURA.
       PROCESSAMENTO.
           PERFORM ROTINA-CALCULO.
           PERFORM PREPARAR-LINHA-DETALHE.
           IF  W-CONT-LINHA > 49
               PERFORM  ROTINA-CABECALHO.
           PERFORM IMPRIMIR-DETALHE.
           PERFORM LEITURA.
       FINALIZACAO.
           PERFORM IMPRIMIR-MEDIA
           CLOSE   ARQ-PROD.
       LEITURA.
           READ  ARQ-PROD.
       ROTINA-CALCULO.
           ADD  PRECO-PROD  TO  W-ACUM-PRECO.
           ADD  1  TO  W-CONT-REGISTRO.
       PREPARAR-LINHA-DETALHE.
           MOVE COD-PROD    TO COD-DET.
           MOVE DESCRI-PROD TO DESCRI-DET.
           MOVE PRECO-PROD  TO PRECO-DET.
       ROTINA-CABECALHO.
           ADD 1 TO W-CONT-PAGINA.
           MOVE W-CONT-PAGINA TO  PAG-CAB.
           WRITE REG-IMP FROM CABEC1 AFTER PAGE.
           WRITE REG-IMP FROM CABEC2 AFTER 2 LINES.
           MOVE SPACES TO REG-IMP.
           WRITE REG-IMP AFTER 1 LINES.
           MOVE  4  TO  W-CONT-LINHA.
       IMPRIMIR-DETALHE.
           WRITE  REG-IMP  FROM  DETALHE AFTER 1 LINES.
           ADD 1 TO W-CONT-LINHA.
       IMPRIMIR-MEDIA.
           COMPUTE W-MEDIA = W-ACUM-PRECO / W-CONT-REGISTRO.
           MOVE W-MEDIA TO MEDIA-DET.
           WRITE REG-IMP  FROM  MEDIA-TOT AFTER 3 LINES.

       FIM.






