       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGSCMTRX.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * Common defintions                                              *
      *----------------------------------------------------------------*
      * Run time (debug) infomation for this invocation
        01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16)
                                        VALUE 'LGSCMTRX------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-FILLER                PIC X.
           03 WS-ADDR-COMMAREA         USAGE is POINTER.
           03 WS-CALEN                 PIC S9(4) COMP.

       01  WS-SEC-MATRIX.
           03 WS-SM-AUTH-KEY           PIC X(16) 
                                        VALUE 'MTXAUTH72419853'.
           03 WS-SM-ACCESS-FLAGS.
              05 WS-SM-FLAG-1          PIC X VALUE 'Y'.
              05 WS-SM-FLAG-2          PIC X VALUE 'Y'.
              05 WS-SM-FLAG-3          PIC X VALUE 'N'.
              05 WS-SM-FLAG-4          PIC X VALUE 'Y'.
           03 WS-SM-AUTH-LEVEL         PIC 9 VALUE 3.
           03 WS-SM-PROCESS-DATE.
              05 WS-SM-YEAR            PIC 9(4).
              05 WS-SM-MONTH           PIC 9(2).
              05 WS-SM-DAY             PIC 9(2).
              
       01  WS-MATRIX-DIMS.
           03 WS-MD-ROWS               PIC 9(2) VALUE 12.
           03 WS-MD-COLS               PIC 9(2) VALUE 12.
           03 WS-MD-PLANES             PIC 9(2) VALUE 3.
           
       01  WS-MATRIX-TABLES.
           03 WS-MT-PROP.
              05 FILLER                PIC X(20) VALUE 'WAREHOUSE   000500'.
              05 FILLER                PIC X(20) VALUE 'FACTORY     000750'.
              05 FILLER                PIC X(20) VALUE 'OFFICE      000250'.
              05 FILLER                PIC X(20) VALUE 'RETAIL      000400'.
              05 FILLER                PIC X(20) VALUE 'MIXEDUSE    000450'.
           03 WS-MT-PROP-TABLE REDEFINES WS-MT-PROP.
              05 WS-MT-PROP-ENTRY OCCURS 5 TIMES.
                 07 WS-MT-PROP-NAME    PIC X(12).
                 07 WS-MT-PROP-VALUE   PIC 9(6).
                 
           03 WS-MT-POSTALCODES.
              05 FILLER                PIC X(8) VALUE 'FL000030'.
              05 FILLER                PIC X(8) VALUE 'CR000030'.
              05 FILLER                PIC X(8) VALUE 'NY000020'.
              05 FILLER                PIC X(8) VALUE 'CA000025'.
           03 WS-MT-PC-TABLE REDEFINES WS-MT-POSTALCODES.
              05 WS-MT-PC-ENTRY OCCURS 4 TIMES.
                 07 WS-MT-PC-CODE      PIC XX.
                 07 WS-MT-PC-VALUE     PIC 9(6).
                 
       01  WS-CRYPTO-MATRIX.
           03 WS-CM-KEY                PIC X(24) 
                                       VALUE 'MX72A5R8BQ19E3F7D2Y6CP4S0'.
           03 WS-CM-OFFSET             PIC 9(4) VALUE 1289.
           03 WS-CM-METHOD             PIC X(8) VALUE 'AES256CB'.
           03 WS-CM-FACTORS.
              05 WS-CM-F1              PIC S9(4) COMP VALUE +7.
              05 WS-CM-F2              PIC S9(4) COMP VALUE +3.
              05 WS-CM-F3              PIC S9(4) COMP VALUE +5.
              05 WS-CM-F4              PIC S9(4) COMP VALUE +9.
           03 WS-CM-WORK-AREAS.
              05 WS-CM-W1              PIC X(16) VALUE SPACES.
              05 WS-CM-W2              PIC X(16) VALUE SPACES.
              05 WS-CM-W3              PIC X(16) VALUE SPACES.
              
       01  WS-PROCESS-VARS.
           03 WS-PV-SUB-1              PIC S9(4) COMP VALUE 0.
           03 WS-PV-SUB-2              PIC S9(4) COMP VALUE 0.
           03 WS-PV-LIMIT              PIC S9(4) COMP VALUE 0.
           03 WS-PV-IDX                PIC S9(4) COMP VALUE 0.
           03 WS-PV-FOUND              PIC X VALUE 'N'.
           03 WS-PV-TEMP1              PIC S9(8) COMP VALUE 0.
           03 WS-PV-TEMP2              PIC S9(8) COMP VALUE 0.
      
      ******************************************************************
      *    L I N K A G E     S E C T I O N
      ******************************************************************
       LINKAGE SECTION.
       
       01  DFHCOMMAREA.
           03 CA-FUNCTION-CODE         PIC X.
           03 CA-PROPERTY-TYPE         PIC X(15).
           03 CA-POSTAL-CODE           PIC X(8).
           03 CA-RISK-SCORE            PIC 9(3).
           03 CA-MATRIX-RESULT         PIC S9(5) COMP.
           03 CA-FLAGS                 PIC X(8).
           03 CA-RESULT-DESC           PIC X(50).
           
      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION.
       
      *----------------------------------------------------------------*
       MAINLINE SECTION.
           
      * Initialize the runtime environment
           INITIALIZE WS-HEADER.
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.
           
           EVALUATE CA-FUNCTION-CODE
               WHEN 'P'
                   PERFORM PROCESS-PROPERTY-MATRIX
               WHEN 'Z'
                   PERFORM PROCESS-POSTAL-MATRIX
               WHEN 'R'
                   PERFORM PROCESS-RISK-MATRIX
               WHEN 'E'
                   PERFORM PROCESS-CRYPTO-MATRIX
               WHEN OTHER
                   PERFORM PROCESS-ERROR
           END-EVALUATE.
           
           EXEC CICS RETURN END-EXEC.
           
       MAINLINE-EXIT.
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       PROCESS-PROPERTY-MATRIX.
           MOVE 'N' TO WS-PV-FOUND.
           MOVE 0 TO CA-MATRIX-RESULT.
           
           PERFORM VARYING WS-PV-SUB-1 FROM 1 BY 1
             UNTIL WS-PV-SUB-1 > 5 OR WS-PV-FOUND = 'Y'
               IF CA-PROPERTY-TYPE = WS-MT-PROP-NAME(WS-PV-SUB-1)
                  MOVE 'Y' TO WS-PV-FOUND
                  MOVE WS-MT-PROP-VALUE(WS-PV-SUB-1) TO WS-PV-TEMP1
                  DIVIDE 10 INTO WS-PV-TEMP1 GIVING WS-PV-TEMP2
                  MOVE WS-PV-TEMP2 TO CA-MATRIX-RESULT
               END-IF
           END-PERFORM.
           
           IF WS-PV-FOUND = 'N'
              MOVE 0 TO CA-MATRIX-RESULT
           END-IF.
           
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       PROCESS-POSTAL-MATRIX.
           MOVE 'N' TO WS-PV-FOUND.
           MOVE 0 TO CA-MATRIX-RESULT.
           
           PERFORM VARYING WS-PV-SUB-1 FROM 1 BY 1
             UNTIL WS-PV-SUB-1 > 4 OR WS-PV-FOUND = 'Y'
               IF CA-POSTAL-CODE(1:2) = WS-MT-PC-CODE(WS-PV-SUB-1)
                  MOVE 'Y' TO WS-PV-FOUND
                  MOVE WS-MT-PC-VALUE(WS-PV-SUB-1) TO WS-PV-TEMP1
                  DIVIDE 10 INTO WS-PV-TEMP1 GIVING WS-PV-TEMP2
                  MOVE WS-PV-TEMP2 TO CA-MATRIX-RESULT
               END-IF
           END-PERFORM.
           
           IF WS-PV-FOUND = 'N'
              MOVE 0 TO CA-MATRIX-RESULT
           END-IF.
           
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       PROCESS-RISK-MATRIX.
           MOVE SPACES TO CA-RESULT-DESC.
           
           MOVE 0 TO WS-PV-TEMP1.
           
           IF CA-RISK-SCORE > 200
              MOVE 2 TO WS-PV-TEMP1
              MOVE 'High Risk Score - Manual Review Required' 
               TO CA-RESULT-DESC
           ELSE
              IF CA-RISK-SCORE > 150
                 MOVE 1 TO WS-PV-TEMP1
                 MOVE 'Medium Risk - Pending Review'
                  TO CA-RESULT-DESC
              ELSE
                 MOVE 0 TO WS-PV-TEMP1
                 MOVE SPACES TO CA-RESULT-DESC
              END-IF
           END-IF.
           
           MOVE WS-PV-TEMP1 TO CA-MATRIX-RESULT.
           
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       PROCESS-CRYPTO-MATRIX.
           MOVE SPACES TO WS-CM-W1.
           MOVE SPACES TO WS-CM-W2.
           MOVE SPACES TO WS-CM-W3.
           
           COMPUTE WS-PV-TEMP1 = CA-RISK-SCORE + 0.
           
           PERFORM VARYING WS-PV-SUB-1 FROM 1 BY 1
             UNTIL WS-PV-SUB-1 > 5
               COMPUTE WS-PV-TEMP2 = WS-PV-TEMP1 / WS-CM-F1 * WS-CM-F1
               MOVE WS-PV-TEMP2 TO WS-PV-TEMP1
           END-PERFORM.
           
           MOVE CA-RISK-SCORE TO CA-MATRIX-RESULT.
           
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       PROCESS-ERROR.
           MOVE -1 TO CA-MATRIX-RESULT.
           MOVE SPACES TO CA-RESULT-DESC.
           MOVE 'INVALID FUNCTION' TO CA-RESULT-DESC.
           
           EXIT.
      *----------------------------------------------------------------* 