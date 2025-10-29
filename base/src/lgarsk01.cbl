       IDENTIFICATION DIVISION.
       PROGRAM-ID. RISKPROG.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO INFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-INPUT-STATUS.
           SELECT OUTPUT-FILE ASSIGN TO OUTFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUTPUT-STATUS.
           SELECT ERROR-FILE ASSIGN TO ERRFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ERROR-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 400 CHARACTERS.
       01  INPUT-RECORD.
           05 IN-POLICY-NUM         PIC X(10).
           05 IN-PROPERTY-TYPE      PIC X(15).
           05 IN-ADDRESS            PIC X(255).
           05 IN-ZIPCODE            PIC X(8).
           05 IN-FR-PR         PIC 9(2).
           05 IN-CR-PR        PIC 9(2).
           05 IN-FL-PR        PIC 9(2).
           05 IN-WEATHER-PERIL      PIC 9(2).
           05 IN-CLAIM-COUNT        PIC 9(3).
           05 IN-TOTAL-CLAIMS       PIC 9(9).

       FD  OUTPUT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 100 CHARACTERS.
       01  OUTPUT-RECORD.
           05 OUT-POLICY-NUM        PIC X(10).
           05 OUT-RISK-SCORE        PIC 9(3)V99.
           05 OUT-RISK-CATEGORY     PIC X(10).
           05 FILLER                PIC X(75).

       FD  ERROR-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 100 CHARACTERS.
       01  ERROR-RECORD.
           05 ERR-POLICY-NUM        PIC X(10).
           05 ERR-MESSAGE           PIC X(90).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-INPUT-STATUS       PIC X(2).
           05 WS-OUTPUT-STATUS      PIC X(2).
           05 WS-ERROR-STATUS       PIC X(2).

       01  WS-RISK-CALCS.
           05 WS-BS-RS          PIC 9(3)V99.
           05 WS-CL-F       PIC 9(1)V99.
           05 WS-LOC-F    PIC 9(1)V99.
           05 WS-F-RSK         PIC 9(3)V99.

       01  WS-EOF                   PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT
           PERFORM 2000-PROCESS UNTIL WS-EOF = 'Y'
           PERFORM 3000-CLOSE
           GOBACK.

       1000-INIT.
           OPEN INPUT  INPUT-FILE
                OUTPUT OUTPUT-FILE
                OUTPUT ERROR-FILE
           IF WS-INPUT-STATUS NOT = '00'
               DISPLAY 'INPUT FILE OPEN ERROR: ' WS-INPUT-STATUS
               MOVE 'Y' TO WS-EOF
           END-IF.

       2000-PROCESS.
           READ INPUT-FILE
               AT END MOVE 'Y' TO WS-EOF
               GO TO 2000-EXIT
           END-READ

           IF WS-INPUT-STATUS NOT = '00'
               MOVE IN-POLICY-NUM TO ERR-POLICY-NUM
               MOVE 'ERROR READING RECORD' TO ERR-MESSAGE
               WRITE ERROR-RECORD
               GO TO 2000-EXIT
           END-IF

           PERFORM 2100-VALIDATE-DATA
           PERFORM 2200-CALCULATE-RISK
           PERFORM 2300-WRITE-OUTPUT

           .
       2000-EXIT.
           EXIT.

       2100-VALIDATE-DATA.
           IF IN-POLICY-NUM = SPACES
               MOVE 'INVALID POLICY NUMBER' TO ERR-MESSAGE
               WRITE ERROR-RECORD
               GO TO 2000-EXIT
           END-IF.

       2200-CALCULATE-RISK.
           EVALUATE IN-PROPERTY-TYPE
               WHEN 'OFFICE'
                   MOVE 1.00 TO WS-BS-RS
               WHEN 'RETAIL'
                   MOVE 1.25 TO WS-BS-RS
               WHEN 'WAREHOUSE'
                   MOVE 1.50 TO WS-BS-RS
               WHEN 'INDUSTRIAL'
                   MOVE 2.00 TO WS-BS-RS
               WHEN OTHER
                   MOVE 1.75 TO WS-BS-RS
           END-EVALUATE

           IF IN-CLAIM-COUNT = 0
               MOVE 0.80 TO WS-CL-F
           ELSE IF IN-CLAIM-COUNT <= 2
               MOVE 1.30 TO WS-CL-F
           ELSE
               MOVE 1.50 TO WS-CL-F
           END-IF

           COMPUTE WS-LOC-F = 1 +
               (IN-FR-PR * 0.2) +
               (IN-CR-PR * 0.2) +
               (IN-FL-PR * 0.3) +
               (IN-WE-PR * 0.2)

           COMPUTE WS-F-RSK ROUNDED =
               WS-BS-RS * WS-CL-F * WS-LOC-F

           IF WS-F-RSK > 9.99
               MOVE 9.99 TO WS-F-RSK
           END-IF.

       2300-WRITE-OUTPUT.
           MOVE IN-POLICY-NUM TO OUT-POLICY-NUM
           MOVE WS-F-RSK TO OUT-RISK-SCORE
      * Set risk category
           EVALUATE TRUE
               WHEN WS-F-RSK < 3.00
                   MOVE 'LOW      ' TO OUT-RISK-CATEGORY
               WHEN WS-F-RSK < 6.00
                   MOVE 'MEDIUM   ' TO OUT-RISK-CATEGORY
               WHEN OTHER
                   MOVE 'HIGH     ' TO OUT-RISK-CATEGORY
           END-EVALUATE
           WRITE OUTPUT-RECORD.

       3000-CLOSE.
           CLOSE INPUT-FILE
                 OUTPUT-FILE
                 ERROR-FILE.