       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGAPDB01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'INPUT.DAT'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-IN-STAT.
           
           SELECT OUTPUT-FILE ASSIGN TO 'OUTPUT.DAT'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-OUT-STAT.
           
           SELECT CONFIG-FILE ASSIGN TO 'CONFIG.DAT'
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS CONFIG-KEY
                  FILE STATUS IS WS-CONFIG-STAT.
                  
           SELECT RATE-FILE ASSIGN TO 'RATES.DAT'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-RATE-STAT.
                  
           SELECT SUMMARY-FILE ASSIGN TO 'SUMMARY.DAT'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-SUM-STAT.

       DATA DIVISION.
       
       FILE SECTION.
       FD  INPUT-FILE.
           COPY INPUTREC2.

       FD  OUTPUT-FILE.
           COPY OUTPUTREC.
           
       FD  CONFIG-FILE.
       01  CONFIG-RECORD.
           05 CONFIG-KEY               PIC X(20).
           05 CONFIG-VALUE             PIC X(100).
           05 CONFIG-TYPE              PIC X(1).
              88 NUMERIC-CONFIG        VALUE 'N'.
              88 TEXT-CONFIG           VALUE 'T'.
              88 DATE-CONFIG           VALUE 'D'.

       FD  RATE-FILE.
       01  RATE-RECORD.
           05 RATE-KEY.
              10 RATE-TERRITORY        PIC X(5).
              10 RATE-CONSTRUCTION     PIC X(3).
              10 RATE-OCCUPANCY        PIC X(5).
              10 RATE-PERIL            PIC X(2).
           05 RATE-DATA.
              10 RATE-BASE-RATE        PIC V9(6).
              10 RATE-MIN-PREMIUM      PIC 9(6)V99.
              10 RATE-MAX-PREMIUM      PIC 9(7)V99.
              10 RATE-EFFECTIVE-DATE   PIC 9(8).
              10 RATE-EXPIRY-DATE      PIC 9(8).
              
       FD  SUMMARY-FILE.
       01  SUMMARY-RECORD             PIC X(132).

       WORKING-STORAGE SECTION.
           COPY WORKSTOR.

       01  WS-SUM-STAT                 PIC X(2).
           88 SUMMARY-OK               VALUE '00'.

       01  WS-ENHANCED-COUNTERS.
           05 WS-APPROVED-CNT          PIC 9(6) VALUE ZERO.
           05 WS-PENDING-CNT           PIC 9(6) VALUE ZERO.
           05 WS-TOTAL-PREMIUM-AMT     PIC 9(12)V99 VALUE ZERO.
           05 WS-AVG-RISK-SCORE        PIC 999V99 VALUE ZERO.
           05 WS-HIGH-RISK-CNT         PIC 9(6) VALUE ZERO.

       01  WS-ACTUARIAL-INTERFACE.
           COPY LGAPACT.

       01  WS-CONFIG-VALUES.
           05 WS-MAX-RISK-SCORE        PIC 999 VALUE 250.
           05 WS-MIN-PREMIUM           PIC 9(6)V99 VALUE 500.00.
           05 WS-MAX-TIV               PIC 9(10)V99 VALUE 50000000.00.
           05 WS-PROCESSING-DATE       PIC 9(8).

       PROCEDURE DIVISION.

       P001.
           PERFORM P002-INITIALIZE
           PERFORM P003-LOAD-CONFIG
           PERFORM P005-OPEN-FILES
           PERFORM P006-PROCESS-RECORDS
           PERFORM P014-CLOSE-FILES
           PERFORM P015-GENERATE-SUMMARY
           PERFORM P016-DISPLAY-STATS
           STOP RUN.

       P002-INITIALIZE.
           DISPLAY 'Enhanced Policy Premium Calculator Starting...'
           DISPLAY 'Version 2.1 - With Actuarial Calculations'
           INITIALIZE WS-PROCESSING-COUNTERS
           INITIALIZE WS-ENHANCED-COUNTERS
           INITIALIZE WS-RISK-ANALYSIS
           INITIALIZE WS-ACTUARIAL-DATA
           INITIALIZE WS-PREMIUM-BREAKDOWN
           INITIALIZE WS-DECISION-DATA
           
           ACCEPT WS-PROCESSING-DATE FROM DATE YYYYMMDD.

       P003-LOAD-CONFIG.
           OPEN INPUT CONFIG-FILE
           IF NOT CONFIG-OK
               DISPLAY 'Warning: Config file not available - using defaults'
               PERFORM P004-SET-DEFAULTS
           ELSE
               PERFORM P004-READ-CONFIG-VALUES
               CLOSE CONFIG-FILE
           END-IF.

       P004-SET-DEFAULTS.
           MOVE 'DEFAULT CONFIG VALUES LOADED' TO WS-UW-NOTES.

       P004-READ-CONFIG-VALUES.
           MOVE 'MAX_RISK_SCORE' TO CONFIG-KEY
           READ CONFIG-FILE
           IF CONFIG-OK AND NUMERIC-CONFIG
               MOVE FUNCTION NUMVAL(CONFIG-VALUE) TO WS-MAX-RISK-SCORE
           END-IF
           
           MOVE 'MIN_PREMIUM' TO CONFIG-KEY
           READ CONFIG-FILE
           IF CONFIG-OK AND NUMERIC-CONFIG
               MOVE FUNCTION NUMVAL(CONFIG-VALUE) TO WS-MIN-PREMIUM
           END-IF.

       P005-OPEN-FILES.
           PERFORM P005A-OPEN-INPUT
           PERFORM P005B-OPEN-OUTPUT
           PERFORM P005C-OPEN-SUMMARY
           PERFORM P005D-WRITE-HEADERS.

       P005A-OPEN-INPUT.
           OPEN INPUT INPUT-FILE
           IF NOT INPUT-OK
               DISPLAY 'Error opening input file: ' WS-IN-STAT
               STOP RUN
           END-IF.

       P005B-OPEN-OUTPUT.
           OPEN OUTPUT OUTPUT-FILE
           IF NOT OUTPUT-OK
               DISPLAY 'Error opening output file: ' WS-OUT-STAT
               STOP RUN
           END-IF.

       P005C-OPEN-SUMMARY.
           OPEN OUTPUT SUMMARY-FILE
           IF NOT SUMMARY-OK
               DISPLAY 'Warning: Cannot open summary file: ' WS-SUM-STAT
           END-IF.

       P005D-WRITE-HEADERS.
           MOVE 'CUSTOMER   ' TO OUT-CUSTOMER-NUM
           MOVE 'PROPERTY-TYPE   ' TO OUT-PROPERTY-TYPE
           MOVE 'POSTCODE' TO OUT-POSTCODE
           MOVE 'RSK' TO OUT-RISK-SCORE
           MOVE 'FIRE-PREM' TO OUT-FIRE-PREMIUM
           MOVE 'CRIME-PREM' TO OUT-CRIME-PREMIUM
           MOVE 'FLOOD-PREM' TO OUT-FLOOD-PREMIUM
           MOVE 'WEATHER-PREM' TO OUT-WEATHER-PREMIUM
           MOVE 'TOTAL-PREMIUM' TO OUT-TOTAL-PREMIUM
           MOVE 'STATUS' TO OUT-STATUS
           MOVE 'REJECTION REASON' TO OUT-REJECT-REASON
           WRITE OUTPUT-RECORD.

       P006-PROCESS-RECORDS.
           PERFORM P007-READ-INPUT
           PERFORM UNTIL INPUT-EOF
               ADD 1 TO WS-REC-CNT
               PERFORM P008-VALIDATE-INPUT-RECORD
               IF WS-ERROR-COUNT = ZERO
                   PERFORM P009-PROCESS-VALID-RECORD
               ELSE
                   PERFORM P010-PROCESS-ERROR-RECORD
               END-IF
               PERFORM P007-READ-INPUT
           END-PERFORM.

       P007-READ-INPUT.
           READ INPUT-FILE
           END-READ.

       P008-VALIDATE-INPUT-RECORD.
           INITIALIZE WS-ERROR-HANDLING
           
           IF NOT COMMERCIAL-POLICY AND 
              NOT PERSONAL-POLICY AND 
              NOT FARM-POLICY
               PERFORM P008A-LOG-ERROR WITH 
                   'POL001' 'F' 'IN-POLICY-TYPE' 
                   'Invalid Policy Type'
           END-IF
           
           IF IN-CUSTOMER-NUM = SPACES
               PERFORM P008A-LOG-ERROR WITH 
                   'CUS001' 'F' 'IN-CUSTOMER-NUM' 
                   'Customer Number Required'
           END-IF
           
           IF IN-BUILDING-LIMIT = ZERO AND 
              IN-CONTENTS-LIMIT = ZERO
               PERFORM P008A-LOG-ERROR WITH 
                   'COV001' 'F' 'COVERAGE-LIMITS' 
                   'At least one coverage limit required'
           END-IF
           
           IF IN-BUILDING-LIMIT + IN-CONTENTS-LIMIT + 
              IN-BI-LIMIT > WS-MAX-TIV
               PERFORM P008A-LOG-ERROR WITH 
                   'COV002' 'W' 'COVERAGE-LIMITS' 
                   'Total coverage exceeds maximum TIV'
           END-IF.

       P008A-LOG-ERROR.
           ADD 1 TO WS-ERROR-COUNT
           SET ERR-IDX TO WS-ERROR-COUNT
           MOVE WS-ERROR-CODE TO WS-ERROR-CODE (ERR-IDX)
           MOVE WS-ERROR-SEVERITY TO WS-ERROR-SEVERITY (ERR-IDX)
           MOVE WS-ERROR-FIELD TO WS-ERROR-FIELD (ERR-IDX)
           MOVE WS-ERROR-MESSAGE TO WS-ERROR-MESSAGE (ERR-IDX).

       P009-PROCESS-VALID-RECORD.
           IF COMMERCIAL-POLICY
               PERFORM P011-PROCESS-COMMERCIAL
               ADD 1 TO WS-PROC-CNT
           ELSE
               PERFORM P012-PROCESS-NON-COMMERCIAL
               ADD 1 TO WS-ERR-CNT
           END-IF.

       P010-PROCESS-ERROR-RECORD.
           MOVE IN-CUSTOMER-NUM TO OUT-CUSTOMER-NUM
           MOVE IN-PROPERTY-TYPE TO OUT-PROPERTY-TYPE
           MOVE IN-POSTCODE TO OUT-POSTCODE
           MOVE ZERO TO OUT-RISK-SCORE
           MOVE ZERO TO OUT-FIRE-PREMIUM
           MOVE ZERO TO OUT-CRIME-PREMIUM
           MOVE ZERO TO OUT-FLOOD-PREMIUM
           MOVE ZERO TO OUT-WEATHER-PREMIUM
           MOVE ZERO TO OUT-TOTAL-PREMIUM
           MOVE 'ERROR' TO OUT-STATUS
           MOVE WS-ERROR-MESSAGE (1) TO OUT-REJECT-REASON
           WRITE OUTPUT-RECORD
           ADD 1 TO WS-ERR-CNT.

       P011-PROCESS-COMMERCIAL.
           PERFORM P011A-CALCULATE-RISK-SCORE
           PERFORM P011B-BASIC-PREMIUM-CALC
           IF WS-STAT = 0
               PERFORM P011C-ENHANCED-ACTUARIAL-CALC
           END-IF
           PERFORM P011D-APPLY-BUSINESS-RULES
           PERFORM P011E-WRITE-OUTPUT-RECORD
           PERFORM P011F-UPDATE-STATISTICS.

       P011A-CALCULATE-RISK-SCORE.
           CALL 'LGAPDB02' USING IN-PROPERTY-TYPE, IN-POSTCODE, 
                                IN-LATITUDE, IN-LONGITUDE,
                                IN-BUILDING-LIMIT, IN-CONTENTS-LIMIT,
                                IN-FLOOD-COVERAGE, IN-WEATHER-COVERAGE,
                                IN-CUSTOMER-HISTORY, WS-BASE-RISK-SCR.

       P011B-BASIC-PREMIUM-CALC.
           CALL 'LGAPDB03' USING WS-BASE-RISK-SCR, IN-FIRE-PERIL, 
                                IN-CRIME-PERIL, IN-FLOOD-PERIL, 
                                IN-WEATHER-PERIL, WS-STAT,
                                WS-STAT-DESC, WS-REJ-RSN, WS-FR-PREM,
                                WS-CR-PREM, WS-FL-PREM, WS-WE-PREM,
                                WS-TOT-PREM, WS-DISC-FACT.

       P011C-ENHANCED-ACTUARIAL-CALC.
      *    Prepare input structure for actuarial calculation
           MOVE IN-CUSTOMER-NUM TO LK-CUSTOMER-NUM
           MOVE WS-BASE-RISK-SCR TO LK-RISK-SCORE
           MOVE IN-PROPERTY-TYPE TO LK-PROPERTY-TYPE
           MOVE IN-TERRITORY-CODE TO LK-TERRITORY
           MOVE IN-CONSTRUCTION-TYPE TO LK-CONSTRUCTION-TYPE
           MOVE IN-OCCUPANCY-CODE TO LK-OCCUPANCY-CODE
           MOVE IN-SPRINKLER-IND TO LK-PROTECTION-CLASS
           MOVE IN-YEAR-BUILT TO LK-YEAR-BUILT
           MOVE IN-SQUARE-FOOTAGE TO LK-SQUARE-FOOTAGE
           MOVE IN-YEARS-IN-BUSINESS TO LK-YEARS-IN-BUSINESS
           MOVE IN-CLAIMS-COUNT-3YR TO LK-CLAIMS-COUNT-5YR
           MOVE IN-CLAIMS-AMOUNT-3YR TO LK-CLAIMS-AMOUNT-5YR
           
      *    Set coverage data
           MOVE IN-BUILDING-LIMIT TO LK-BUILDING-LIMIT
           MOVE IN-CONTENTS-LIMIT TO LK-CONTENTS-LIMIT
           MOVE IN-BI-LIMIT TO LK-BI-LIMIT
           MOVE IN-FIRE-DEDUCTIBLE TO LK-FIRE-DEDUCTIBLE
           MOVE IN-WIND-DEDUCTIBLE TO LK-WIND-DEDUCTIBLE
           MOVE IN-FLOOD-DEDUCTIBLE TO LK-FLOOD-DEDUCTIBLE
           MOVE IN-OTHER-DEDUCTIBLE TO LK-OTHER-DEDUCTIBLE
           MOVE IN-FIRE-PERIL TO LK-FIRE-PERIL
           MOVE IN-CRIME-PERIL TO LK-CRIME-PERIL
           MOVE IN-FLOOD-PERIL TO LK-FLOOD-PERIL
           MOVE IN-WEATHER-PERIL TO LK-WEATHER-PERIL
           
      *    Call advanced actuarial calculation program (only for approved cases)
           IF WS-TOT-PREM > WS-MIN-PREMIUM
               CALL 'LGAPDB04' USING LK-INPUT-DATA, LK-COVERAGE-DATA, 
                                    LK-OUTPUT-RESULTS
               
      *        Update with enhanced calculations if successful
               IF LK-TOTAL-PREMIUM > WS-TOT-PREM
                   MOVE LK-FIRE-PREMIUM TO WS-FR-PREM
                   MOVE LK-CRIME-PREMIUM TO WS-CR-PREM
                   MOVE LK-FLOOD-PREMIUM TO WS-FL-PREM
                   MOVE LK-WEATHER-PREMIUM TO WS-WE-PREM
                   MOVE LK-TOTAL-PREMIUM TO WS-TOT-PREM
                   MOVE LK-EXPERIENCE-MOD TO WS-EXPERIENCE-MOD
               END-IF
           END-IF.

       P011D-APPLY-BUSINESS-RULES.
      *    Determine underwriting decision based on enhanced criteria
           EVALUATE TRUE
               WHEN WS-BASE-RISK-SCR > WS-MAX-RISK-SCORE
                   MOVE 2 TO WS-STAT
                   MOVE 'REJECTED' TO WS-STAT-DESC
                   MOVE 'Risk score exceeds maximum acceptable level' 
                        TO WS-REJ-RSN
               WHEN WS-TOT-PREM < WS-MIN-PREMIUM
                   MOVE 1 TO WS-STAT
                   MOVE 'PENDING' TO WS-STAT-DESC
                   MOVE 'Premium below minimum - requires review'
                        TO WS-REJ-RSN
               WHEN WS-BASE-RISK-SCR > 180
                   MOVE 1 TO WS-STAT
                   MOVE 'PENDING' TO WS-STAT-DESC
                   MOVE 'High risk - underwriter review required'
                        TO WS-REJ-RSN
               WHEN OTHER
                   MOVE 0 TO WS-STAT
                   MOVE 'APPROVED' TO WS-STAT-DESC
                   MOVE SPACES TO WS-REJ-RSN
           END-EVALUATE.

       P011E-WRITE-OUTPUT-RECORD.
           MOVE IN-CUSTOMER-NUM TO OUT-CUSTOMER-NUM
           MOVE IN-PROPERTY-TYPE TO OUT-PROPERTY-TYPE
           MOVE IN-POSTCODE TO OUT-POSTCODE
           MOVE WS-BASE-RISK-SCR TO OUT-RISK-SCORE
           MOVE WS-FR-PREM TO OUT-FIRE-PREMIUM
           MOVE WS-CR-PREM TO OUT-CRIME-PREMIUM
           MOVE WS-FL-PREM TO OUT-FLOOD-PREMIUM
           MOVE WS-WE-PREM TO OUT-WEATHER-PREMIUM
           MOVE WS-TOT-PREM TO OUT-TOTAL-PREMIUM
           MOVE WS-STAT-DESC TO OUT-STATUS
           MOVE WS-REJ-RSN TO OUT-REJECT-REASON
           WRITE OUTPUT-RECORD.

       P011F-UPDATE-STATISTICS.
           ADD WS-TOT-PREM TO WS-TOTAL-PREMIUM-AMT
           ADD WS-BASE-RISK-SCR TO WS-CONTROL-TOTALS
           
           EVALUATE WS-STAT
               WHEN 0 ADD 1 TO WS-APPROVED-CNT
               WHEN 1 ADD 1 TO WS-PENDING-CNT
               WHEN 2 ADD 1 TO WS-REJECTED-CNT
           END-EVALUATE
           
           IF WS-BASE-RISK-SCR > 200
               ADD 1 TO WS-HIGH-RISK-CNT
           END-IF.

       P012-PROCESS-NON-COMMERCIAL.
           MOVE IN-CUSTOMER-NUM TO OUT-CUSTOMER-NUM
           MOVE IN-PROPERTY-TYPE TO OUT-PROPERTY-TYPE
           MOVE IN-POSTCODE TO OUT-POSTCODE
           MOVE ZERO TO OUT-RISK-SCORE
           MOVE ZERO TO OUT-FIRE-PREMIUM
           MOVE ZERO TO OUT-CRIME-PREMIUM
           MOVE ZERO TO OUT-FLOOD-PREMIUM
           MOVE ZERO TO OUT-WEATHER-PREMIUM
           MOVE ZERO TO OUT-TOTAL-PREMIUM
           MOVE 'UNSUPPORTED' TO OUT-STATUS
           MOVE 'Only Commercial policies supported in this version' 
                TO OUT-REJECT-REASON
           WRITE OUTPUT-RECORD.

       P014-CLOSE-FILES.
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           IF SUMMARY-OK
               CLOSE SUMMARY-FILE
           END-IF.

       P015-GENERATE-SUMMARY.
           IF NOT SUMMARY-OK
               GO TO P015-EXIT
           END-IF
           
           MOVE SPACES TO SUMMARY-RECORD
           STRING 'POLICY PREMIUM CALCULATION SUMMARY' 
                  DELIMITED BY SIZE INTO SUMMARY-RECORD
           WRITE SUMMARY-RECORD
           
           MOVE SPACES TO SUMMARY-RECORD
           STRING 'PROCESSING DATE: ' WS-PROCESSING-DATE
                  DELIMITED BY SIZE INTO SUMMARY-RECORD
           WRITE SUMMARY-RECORD
           
           MOVE SPACES TO SUMMARY-RECORD
           WRITE SUMMARY-RECORD
           
           STRING 'TOTAL RECORDS PROCESSED: ' WS-REC-CNT
                  DELIMITED BY SIZE INTO SUMMARY-RECORD
           WRITE SUMMARY-RECORD
           
           STRING 'POLICIES APPROVED: ' WS-APPROVED-CNT
                  DELIMITED BY SIZE INTO SUMMARY-RECORD
           WRITE SUMMARY-RECORD
           
           STRING 'POLICIES PENDING: ' WS-PENDING-CNT
                  DELIMITED BY SIZE INTO SUMMARY-RECORD
           WRITE SUMMARY-RECORD
           
           STRING 'POLICIES REJECTED: ' WS-REJECTED-CNT
                  DELIMITED BY SIZE INTO SUMMARY-RECORD
           WRITE SUMMARY-RECORD
           
           STRING 'TOTAL PREMIUM AMOUNT: $' WS-TOTAL-PREMIUM-AMT
                  DELIMITED BY SIZE INTO SUMMARY-RECORD
           WRITE SUMMARY-RECORD
           
           IF WS-PROC-CNT > ZERO
               COMPUTE WS-AVG-RISK-SCORE = 
                   WS-CONTROL-TOTALS / WS-PROC-CNT
               STRING 'AVERAGE RISK SCORE: ' WS-AVG-RISK-SCORE
                      DELIMITED BY SIZE INTO SUMMARY-RECORD
               WRITE SUMMARY-RECORD
           END-IF.

       P015-EXIT.
           EXIT.

       P016-DISPLAY-STATS.
           DISPLAY 'Enhanced Processing Complete:'
           DISPLAY 'Total Records Read: ' WS-REC-CNT
           DISPLAY 'Records Processed: ' WS-PROC-CNT
           DISPLAY 'Records Approved: ' WS-APPROVED-CNT
           DISPLAY 'Records Pending: ' WS-PENDING-CNT  
           DISPLAY 'Records Rejected: ' WS-REJECTED-CNT
           DISPLAY 'Error Records: ' WS-ERR-CNT
           DISPLAY 'High Risk Count: ' WS-HIGH-RISK-CNT
           DISPLAY 'Total Premium Generated: $' WS-TOTAL-PREMIUM-AMT
           IF WS-PROC-CNT > ZERO
               DISPLAY 'Average Risk Score: ' WS-AVG-RISK-SCORE
           END-IF.