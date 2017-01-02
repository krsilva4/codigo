      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.    AC220002.
       AUTHOR.  ACCENTURE BRDC RECIFE.
      *================================================================*
      *                      A C C E N T U R E                         *
      *================================================================*
      *    PROGRAMA......: AC220002.                                   *
      *    PROGRAMADOR...: ACCENTURE BRDC RECIFE                       *
      *    ANALISTA......: ACCENTURE BRDC                              *
      *    DATA..........: 18/11/2016                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO......: MANUTENCAO DA TABELA TABPRO                 *
      *----------------------------------------------------------------*
      *    ARQUIVOS         I/O LRECL  DESCRICAO                       *
      *    TABELA TABPRO    I/O  071    DB2ADMIN.TABPRO                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
       FILE-CONTROL.
      *================================================================*
       DATA                            DIVISION.
      *================================================================* 
       FILE                            SECTION.
      *================================================================*
       WORKING-STORAGE                 SECTION.
      *================================================================*
           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.
      *--------------CONECTANDO AO BANCO DE DADOS DB2------------------*
           EXEC SQL
             INCLUDE DCLTBPRO
           END-EXEC.
      * --------VARIAVEIS AUXILIARES DE CALCULO E CONTADORES-----------*
       01 WRK-VARIAVEIS-AUXILIARES.
           03 WRK-ACU-TABPRO           PIC 9(03)       VALUE ZEROS.
           03 WRK-ACU-DELETADOS        PIC 9(03)       VALUE ZEROS.
           03 WRK-ACU-ALTERADOS        PIC 9(03)       VALUE ZEROS.
           03 WRK-CALC-GRUPO           PIC S9(8)V9(2)  USAGE COMP-3.
           03 WRK-PRECO-ANT            PIC S9(8)V9(2)  USAGE COMP-3.
      *---------VARIAVEIS PARA MASCARAS DE CONTADORES------------------*
       01 WRK-VARIAVEIS-PARA-MASCARA.
           03 WRK-MASC-TABPRO          PIC ZZ9.
           03 WRK-MASC-DELETADOS       PIC ZZ9. 
           03 WRK-MASC-ALTERADOS       PIC ZZ9.
      *----------------------------------------------------------------*
            EXEC SQL DECLARE CSR01-TABPRO CURSOR FOR
                SELECT TABPRO_COD_GRUPO,
                       TABPRO_COD_CATEGORIA,
                       TABPRO_COD_PRODUTO,
                       TABPRO_PRECO
                FROM DB2ADMIN.TABPRO
                WHERE TABPRO_COD_GRUPO IN (1,2,3)
            END-EXEC. 
      *----------------------------------------------------------------*
       PROCEDURE                       DIVISION. 
      *  
       000-PRINCIPAL                   SECTION.
           EXEC SQL
               CONNECT TO 'DB2'
           END-EXEC
      *   
           PERFORM 1000-INICIALIZAR    THRU 1000-99-FIM
      *
           PERFORM 2000-PROCESSAR      THRU 2000-99-FIM
           UNTIL SQLCODE EQUAL +100
      *                                 
           PERFORM 3000-TERMINAR       THRU 3000-99-FIM
           PERFORM 3300-FINALIZAR      THRU 3300-99-FIM.                
      *
       000-99-PRINCIPAL-FIM            SECTION. 
           EXIT.                                                      
      *-----------------------------------------------------------------
      * INICIALIZACAO DE VARIAVEIS, LEITURA DA TABELA TABPRO, TESTES
      * DE SQLCODE E  CONNECT DB2.
      *-----------------------------------------------------------------
       1000-INICIALIZAR                SECTION.
           INITIALIZE WRK-VARIAVEIS-AUXILIARES 
                      DCLTABPRO-REGISTRO
      *                
           PERFORM 1100-ABRIR-CURSOR   THRU 1100-99-FIM
           PERFORM 1200-LER-CURSOR     THRU 1200-99-FIM
      *     
           IF SQLCODE EQUAL 100 
               DISPLAY 'CURSOR VAZIO '
               PERFORM 3300-FINALIZAR  THRU 3300-99-FIM
           END-IF. 
      *     
       1000-99-FIM.                    EXIT.    
      *
       1100-ABRIR-CURSOR               SECTION.  
      *    
           EXEC SQL                                                    
               OPEN CSR01-TABPRO                                        
           END-EXEC
      *     
           IF SQLCODE NOT EQUAL ZEROS 
              DISPLAY 'ERRO DE ABERTURA DO CURSOR ' SQLCODE
              PERFORM 3300-FINALIZAR   THRU 3300-99-FIM
           END-IF.   
      *     
       1100-99-FIM.                    EXIT.    
      *
       1200-LER-CURSOR                 SECTION.
           EXEC SQL
                FETCH  CSR01-TABPRO
                INTO  :TABPRO-COD-GRUPO
                ,     :TABPRO-COD-CATEGORIA
                ,     :TABPRO-COD-PRODUTO 
                ,     :TABPRO-PRECO 
           END-EXEC
      *  
           IF SQLCODE NOT EQUAL ZEROS AND
              SQLCODE NOT EQUAL 100 
              DISPLAY 'ERRO DE LEITURA 'SQLCODE
              PERFORM 3300-FINALIZAR   THRU 3300-99-FIM
           END-IF
      *     
           IF SQLCODE EQUAL ZEROS 
              ADD 1                    TO WRK-ACU-TABPRO
           END-IF.
      *
       1200-99-FIM.                    EXIT.    
      * 
      *-----------------------------------------------------------------
      * GRAVACAO DE REGISTRO NA TABELA TABPRO SEGUNDO AS CONDICOES
      *  ABAIXO.
      *-----------------------------------------------------------------
       2000-PROCESSAR                  SECTION.
      *                                                                 
           IF TABPRO-COD-GRUPO EQUAL '3' 
               PERFORM 2100-DELETAR-TABPRO
               DISPLAY 'REGISTRO DELETADO: ' TABPRO-COD-GRUPO ' '
                        TABPRO-COD-CATEGORIA ' ' TABPRO-COD-PRODUTO
           ELSE
               IF TABPRO-COD-GRUPO EQUAL 1 OR 2
                   PERFORM 2200-ALTERA-PRECO 
                   DISPLAY 'REGISTRO ALTERADO: ' TABPRO-COD-GRUPO ' '
                   TABPRO-COD-CATEGORIA ' ' TABPRO-COD-PRODUTO ' ' 
                   ' PRECO ANT: ' WRK-PRECO-ANT ' PRECO ALT: ' 
                   TABPRO-PRECO
               END-IF
           END-IF
      *
           PERFORM 1200-LER-CURSOR     THRU 1200-99-FIM.
      *     
       2000-99-FIM.                    EXIT.    
      *
       2100-DELETAR-TABPRO             SECTION.
      * 
           EXEC SQL
               DELETE FROM DB2ADMIN.TABPRO
               WHERE TABPRO_COD_GRUPO     = :TABPRO-COD-GRUPO
                AND  TABPRO_COD_CATEGORIA = :TABPRO-COD-CATEGORIA
                AND  TABPRO_COD_PRODUTO   = :TABPRO-COD-PRODUTO
                AND  TABPRO_PRECO         = :TABPRO-PRECO          
      *         
           END-EXEC
           IF SQLCODE NOT EQUAL ZEROS 
              DISPLAY 'ERRO DE DELETE: 'SQLCODE
              PERFORM 3300-FINALIZAR  
           ELSE
                ADD 1                  TO WRK-ACU-DELETADOS
           END-IF.   
      * 
       2100-99-FIM.                    EXIT. 
       
       2200-ALTERA-PRECO.
           MOVE TABPRO-PRECO           TO WRK-PRECO-ANT
           IF TABPRO-COD-GRUPO EQUAL 1
               COMPUTE WRK-CALC-GRUPO = (TABPRO-PRECO * 1,10)
           ELSE
               COMPUTE WRK-CALC-GRUPO = (TABPRO-PRECO * 0,95)
           END-IF 
      * 
           MOVE WRK-CALC-GRUPO         TO TABPRO-PRECO
           EXEC SQL
           UPDATE DB2ADMIN.TABPRO 
              SET TABPRO_PRECO           = :TABPRO-PRECO
              WHERE TABPRO_COD_GRUPO     = :TABPRO-COD-GRUPO
                AND TABPRO_COD_CATEGORIA = :TABPRO-COD-CATEGORIA
                AND TABPRO_COD_PRODUTO   = :TABPRO-COD-PRODUTO
           END-EXEC
      *       
           IF SQLCODE NOT EQUAL ZEROS
              DISPLAY 'ERRO DE UPDATE: 'SQLCODE
              PERFORM 3300-FINALIZAR
           ELSE
              ADD 1                    TO WRK-ACU-ALTERADOS
           END-IF.   
      * 
       2200-99-FIM.                    EXIT. 
      * 
      **----------------------------------------------------------------
      * EXECUTANDO TRATAMENTOS FINAIS.
      *-----------------------------------------------------------------
       3000-TERMINAR.
           PERFORM 3100-TRATAMENTOS-FINAIS
           PERFORM 3200-FECHAR-CURSOR.
      *     
       3000-99-FIM.                    EXIT.  
      * 
       3100-TRATAMENTOS-FINAIS.
      * 
           MOVE WRK-ACU-TABPRO         TO WRK-MASC-TABPRO
           MOVE WRK-ACU-ALTERADOS      TO WRK-MASC-ALTERADOS
           MOVE WRK-ACU-DELETADOS      TO WRK-MASC-DELETADOS
      *     
           DISPLAY 'TOTAL DE REGISTROS LIDOS..: 'WRK-MASC-TABPRO 
           DISPLAY 'TOTAL REGISTROS ALTERADOS.: 'WRK-MASC-ALTERADOS
           DISPLAY 'TOTAL REGISTROS DELETADOS : 'WRK-MASC-DELETADOS.
      *     
       3100-99-FIM.                    EXIT.   
      *
       3200-FECHAR-CURSOR.
      *    
           EXEC SQL
              CLOSE CSR01-TABPRO
           END-EXEC
      *     
           IF SQLCODE NOT EQUAL ZEROS
              DISPLAY 'ERRO DE CLOSE: ' SQLCODE
              PERFORM 3300-FINALIZAR
           END-IF   
      *
           EXEC SQL
               COMMIT WORK    
           END-EXEC
      *     
           IF SQLCODE NOT EQUAL ZEROS
              DISPLAY 'ERRO NO COMMIT: ' SQLCODE
              PERFORM 3300-FINALIZAR
           END-IF.
      *     
       3200-99-FIM.                    EXIT.
      * 
       3300-FINALIZAR.
      * 
           GOBACK.
       3300-99-FIM.                    EXIT.
      * 