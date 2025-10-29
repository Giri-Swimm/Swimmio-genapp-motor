       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGAPRPT1.
      *================================================================*
      * PROGRAM: LGAPRPT1 - DAILY PREMIUM SUMMARY REPORT GENERATOR    *
      * PURPOSE: READS PREMIUM OUTPUT FILE AND GENERATES FORMATTED    *
      *          MANAGEMENT REPORTS WITH STATISTICS AND BREAKDOWNS    *
      * AUTHOR:  LGAP DEVELOPMENT TEAM                                 *
      *================================================================*
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'INPUT'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-IN-STAT.
           
           SELECT REPORT-FILE ASSIGN TO 'REPORT'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-RPT-STAT.

       DATA DIVISION.
       
       FILE SECTION.
       FD  INPUT-FILE.
           COPY OUTPUTREC.

       FD  REPORT-FILE.
       01  REPORT-LINE                 PIC X(133).

       WORKING-STORAGE SECTION.
       
      *----------------------------------------------------------------*
      * FILE STATUS FIELDS                                            *
      *----------------------------------------------------------------*
       01  WS-IN-STAT                  PIC X(2).
           88 INPUT-OK                 VALUE '00'.
           88 INPUT-EOF                VALUE '10'.
           
       01  WS-RPT-STAT                 PIC X(2).
           88 REPORT-OK                VALUE '00'.

      *----------------------------------------------------------------*
      * COUNTERS AND ACCUMULATORS                                     *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05 WS-TOTAL-REC             PIC 9(7) VALUE ZERO.
           05 WS-APPROVED-CNT          PIC 9(6) VALUE ZERO.
           05 WS-PENDING-CNT           PIC 9(6) VALUE ZERO.
           05 WS-REJECTED-CNT          PIC 9(6) VALUE ZERO.
           05 WS-ERROR-CNT             PIC 9(6) VALUE ZERO.
           05 WS-HEADER-CNT            PIC 9(2) VALUE ZERO.
           
       01  WS-PREMIUM-TOTALS.
           05 WS-TOTAL-FIRE-PREM       PIC 9(12)V99 VALUE ZERO.
           05 WS-TOTAL-CRIME-PREM      PIC 9(12)V99 VALUE ZERO.
           05 WS-TOTAL-FLOOD-PREM      PIC 9(12)V99 VALUE ZERO.
           05 WS-TOTAL-WEATHER-PREM    PIC 9(12)V99 VALUE ZERO.
           05 WS-GRAND-TOTAL-PREM      PIC 9(13)V99 VALUE ZERO.
           
       01  WS-RISK-ANALYSIS.
           05 WS-TOTAL-RISK-SCORE      PIC 9(9) VALUE ZERO.
           05 WS-AVG-RISK-SCORE        PIC 999V99 VALUE ZERO.
           05 WS-HIGH-RISK-CNT         PIC 9(6) VALUE ZERO.
           05 WS-MED-RISK-CNT          PIC 9(6) VALUE ZERO.
           05 WS-LOW-RISK-CNT          PIC 9(6) VALUE ZERO.

      *----------------------------------------------------------------*
      * WORK AREAS                                                    *
      *----------------------------------------------------------------*
       01  WS-WORK-FIELDS.
           05 WS-RISK-SCORE-NUM        PIC 999.
           05 WS-FIRE-PREM-NUM         PIC 9(8)V99.
           05 WS-CRIME-PREM-NUM        PIC 9(8)V99.
           05 WS-FLOOD-PREM-NUM        PIC 9(8)V99.
           05 WS-WEATHER-PREM-NUM      PIC 9(8)V99.
           05 WS-TOTAL-PREM-NUM        PIC 9(9)V99.
           
       01  WS-DATE-TIME.
           05 WS-CURRENT-DATE          PIC 9(8).
           05 WS-CURRENT-TIME          PIC 9(6).
           05 WS-FORMAT-DATE           PIC X(10).
           05 WS-FORMAT-TIME           PIC X(8).

      *----------------------------------------------------------------*
      * REPORT HEADER LINES                                           *
      *----------------------------------------------------------------*
       01  RPT-HEADER-1.
           05 FILLER                   PIC X(40) VALUE SPACES.
           05 FILLER                   PIC X(53)
              VALUE 'DAILY PREMIUM CALCULATION SUMMARY REPORT'.
           05 FILLER                   PIC X(40) VALUE SPACES.
           
       01  RPT-HEADER-2.
           05 FILLER                   PIC X(5) VALUE 'DATE:'.
           05 RPT-DATE                 PIC X(10).
           05 FILLER                   PIC X(10) VALUE SPACES.
           05 FILLER                   PIC X(5) VALUE 'TIME:'.
           05 RPT-TIME                 PIC X(8).
           05 FILLER                   PIC X(95) VALUE SPACES.
           
       01  RPT-HEADER-3.
           05 FILLER                   PIC X(133) VALUE ALL '='.

      *----------------------------------------------------------------*
      * REPORT DETAIL LINES                                           *
      *----------------------------------------------------------------*
       01  RPT-SECTION-HEADER.
           05 FILLER                   PIC X(3) VALUE SPACES.
           05 RPT-SECTION-TITLE        PIC X(50).
           05 FILLER                   PIC X(80) VALUE SPACES.
           
       01  RPT-DETAIL-LINE.
           05 FILLER                   PIC X(5) VALUE SPACES.
           05 RPT-DETAIL-LABEL         PIC X(40).
           05 RPT-DETAIL-VALUE         PIC X(25).
           05 FILLER                   PIC X(63) VALUE SPACES.
           
       01  RPT-BLANK-LINE              PIC X(133) VALUE SPACES.

       PROCEDURE DIVISION.

       P100-MAIN.
           PERFORM P200-INIT
           PERFORM P300-OPEN-FILES
           PERFORM P400-WRITE-HEADERS
           PERFORM P500-PROCESS-RECORDS
           PERFORM P600-WRITE-SUMMARY
           PERFORM P700-CLOSE-FILES
           STOP RUN.

       P200-INIT.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           PERFORM P210-FORMAT-DATE-TIME
           INITIALIZE WS-COUNTERS
           INITIALIZE WS-PREMIUM-TOTALS
           INITIALIZE WS-RISK-ANALYSIS.

       P210-FORMAT-DATE-TIME.
           STRING WS-CURRENT-DATE(5:2) '/'
                  WS-CURRENT-DATE(7:2) '/'
                  WS-CURRENT-DATE(1:4)
                  DELIMITED BY SIZE INTO WS-FORMAT-DATE
           
           STRING WS-CURRENT-TIME(1:2) ':'
                  WS-CURRENT-TIME(3:2) ':'
                  WS-CURRENT-TIME(5:2)
                  DELIMITED BY SIZE INTO WS-FORMAT-TIME.

       P300-OPEN-FILES.
           OPEN INPUT INPUT-FILE
           IF NOT INPUT-OK
               DISPLAY 'ERROR: Cannot open input file: ' WS-IN-STAT
               STOP RUN
           END-IF
           
           OPEN OUTPUT REPORT-FILE
           IF NOT REPORT-OK
               DISPLAY 'ERROR: Cannot open report file: ' WS-RPT-STAT
               CLOSE INPUT-FILE
               STOP RUN
           END-IF.

       P400-WRITE-HEADERS.
           WRITE REPORT-LINE FROM RPT-HEADER-1 AFTER ADVANCING PAGE
           MOVE WS-FORMAT-DATE TO RPT-DATE
           MOVE WS-FORMAT-TIME TO RPT-TIME
           WRITE REPORT-LINE FROM RPT-HEADER-2 AFTER ADVANCING 2 LINES
           WRITE REPORT-LINE FROM RPT-HEADER-3 AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM RPT-BLANK-LINE AFTER ADVANCING 1 LINE.

       P500-PROCESS-RECORDS.
           PERFORM P510-READ-INPUT
           PERFORM UNTIL INPUT-EOF
               PERFORM P520-PROCESS-RECORD
               PERFORM P510-READ-INPUT
           END-PERFORM.

       P510-READ-INPUT.
           READ INPUT-FILE
           END-READ.

       P520-PROCESS-RECORD.
           ADD 1 TO WS-TOTAL-REC
           
      *    Skip header record
           IF WS-TOTAL-REC = 1
               ADD 1 TO WS-HEADER-CNT
               GO TO P520-EXIT
           END-IF
           
      *    Convert numeric fields
           MOVE FUNCTION NUMVAL(OUT-RISK-SCORE) TO WS-RISK-SCORE-NUM
           MOVE FUNCTION NUMVAL(OUT-FIRE-PREMIUM) TO WS-FIRE-PREM-NUM
           MOVE FUNCTION NUMVAL(OUT-CRIME-PREMIUM) TO WS-CRIME-PREM-NUM
           MOVE FUNCTION NUMVAL(OUT-FLOOD-PREMIUM) TO WS-FLOOD-PREM-NUM
           MOVE FUNCTION NUMVAL(OUT-WEATHER-PREMIUM) TO WS-WEATHER-PREM-NUM
           MOVE FUNCTION NUMVAL(OUT-TOTAL-PREMIUM) TO WS-TOTAL-PREM-NUM
           
      *    Accumulate totals
           ADD WS-FIRE-PREM-NUM TO WS-TOTAL-FIRE-PREM
           ADD WS-CRIME-PREM-NUM TO WS-TOTAL-CRIME-PREM
           ADD WS-FLOOD-PREM-NUM TO WS-TOTAL-FLOOD-PREM
           ADD WS-WEATHER-PREM-NUM TO WS-TOTAL-WEATHER-PREM
           ADD WS-TOTAL-PREM-NUM TO WS-GRAND-TOTAL-PREM
           ADD WS-RISK-SCORE-NUM TO WS-TOTAL-RISK-SCORE
           
      *    Categorize by status
           EVALUATE OUT-STATUS
               WHEN 'APPROVED'
                   ADD 1 TO WS-APPROVED-CNT
               WHEN 'PENDING'
                   ADD 1 TO WS-PENDING-CNT
               WHEN 'REJECTED'
                   ADD 1 TO WS-REJECTED-CNT
               WHEN OTHER
                   ADD 1 TO WS-ERROR-CNT
           END-EVALUATE
           
      *    Categorize by risk level
           EVALUATE TRUE
               WHEN WS-RISK-SCORE-NUM >= 180
                   ADD 1 TO WS-HIGH-RISK-CNT
               WHEN WS-RISK-SCORE-NUM >= 120
                   ADD 1 TO WS-MED-RISK-CNT
               WHEN OTHER
                   ADD 1 TO WS-LOW-RISK-CNT
           END-EVALUATE.
           
       P520-EXIT.
           EXIT.

       P600-WRITE-SUMMARY.
           PERFORM P610-CALC-AVERAGES
           PERFORM P620-WRITE-VOLUME-SECTION
           PERFORM P630-WRITE-STATUS-SECTION
           PERFORM P640-WRITE-PREMIUM-SECTION
           PERFORM P650-WRITE-RISK-SECTION.

       P610-CALC-AVERAGES.
           SUBTRACT WS-HEADER-CNT FROM WS-TOTAL-REC
           IF WS-TOTAL-REC > ZERO
               COMPUTE WS-AVG-RISK-SCORE = 
                   WS-TOTAL-RISK-SCORE / WS-TOTAL-REC
           END-IF.

       P620-WRITE-VOLUME-SECTION.
           MOVE 'PROCESSING VOLUME' TO RPT-SECTION-TITLE
           WRITE REPORT-LINE FROM RPT-SECTION-HEADER 
               AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM RPT-BLANK-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE 'Total Records Processed:' TO RPT-DETAIL-LABEL
           MOVE WS-TOTAL-REC TO RPT-DETAIL-VALUE
           WRITE REPORT-LINE FROM RPT-DETAIL-LINE 
               AFTER ADVANCING 1 LINE.

       P630-WRITE-STATUS-SECTION.
           WRITE REPORT-LINE FROM RPT-BLANK-LINE 
               AFTER ADVANCING 2 LINES
           MOVE 'UNDERWRITING DECISIONS' TO RPT-SECTION-TITLE
           WRITE REPORT-LINE FROM RPT-SECTION-HEADER 
               AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM RPT-BLANK-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE 'Policies Approved:' TO RPT-DETAIL-LABEL
           MOVE WS-APPROVED-CNT TO RPT-DETAIL-VALUE
           WRITE REPORT-LINE FROM RPT-DETAIL-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE 'Policies Pending Review:' TO RPT-DETAIL-LABEL
           MOVE WS-PENDING-CNT TO RPT-DETAIL-VALUE
           WRITE REPORT-LINE FROM RPT-DETAIL-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE 'Policies Rejected:' TO RPT-DETAIL-LABEL
           MOVE WS-REJECTED-CNT TO RPT-DETAIL-VALUE
           WRITE REPORT-LINE FROM RPT-DETAIL-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE 'Errors/Unsupported:' TO RPT-DETAIL-LABEL
           MOVE WS-ERROR-CNT TO RPT-DETAIL-VALUE
           WRITE REPORT-LINE FROM RPT-DETAIL-LINE 
               AFTER ADVANCING 1 LINE.

       P640-WRITE-PREMIUM-SECTION.
           WRITE REPORT-LINE FROM RPT-BLANK-LINE 
               AFTER ADVANCING 2 LINES
           MOVE 'PREMIUM TOTALS' TO RPT-SECTION-TITLE
           WRITE REPORT-LINE FROM RPT-SECTION-HEADER 
               AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM RPT-BLANK-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE 'Total Fire Premium:' TO RPT-DETAIL-LABEL
           MOVE WS-TOTAL-FIRE-PREM TO RPT-DETAIL-VALUE
           WRITE REPORT-LINE FROM RPT-DETAIL-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE 'Total Crime Premium:' TO RPT-DETAIL-LABEL
           MOVE WS-TOTAL-CRIME-PREM TO RPT-DETAIL-VALUE
           WRITE REPORT-LINE FROM RPT-DETAIL-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE 'Total Flood Premium:' TO RPT-DETAIL-LABEL
           MOVE WS-TOTAL-FLOOD-PREM TO RPT-DETAIL-VALUE
           WRITE REPORT-LINE FROM RPT-DETAIL-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE 'Total Weather Premium:' TO RPT-DETAIL-LABEL
           MOVE WS-TOTAL-WEATHER-PREM TO RPT-DETAIL-VALUE
           WRITE REPORT-LINE FROM RPT-DETAIL-LINE 
               AFTER ADVANCING 1 LINE
           
           WRITE REPORT-LINE FROM RPT-BLANK-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE 'GRAND TOTAL PREMIUM:' TO RPT-DETAIL-LABEL
           MOVE WS-GRAND-TOTAL-PREM TO RPT-DETAIL-VALUE
           WRITE REPORT-LINE FROM RPT-DETAIL-LINE 
               AFTER ADVANCING 1 LINE.

       P650-WRITE-RISK-SECTION.
           WRITE REPORT-LINE FROM RPT-BLANK-LINE 
               AFTER ADVANCING 2 LINES
           MOVE 'RISK ANALYSIS' TO RPT-SECTION-TITLE
           WRITE REPORT-LINE FROM RPT-SECTION-HEADER 
               AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM RPT-BLANK-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE 'Average Risk Score:' TO RPT-DETAIL-LABEL
           MOVE WS-AVG-RISK-SCORE TO RPT-DETAIL-VALUE
           WRITE REPORT-LINE FROM RPT-DETAIL-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE 'High Risk Policies (180+):' TO RPT-DETAIL-LABEL
           MOVE WS-HIGH-RISK-CNT TO RPT-DETAIL-VALUE
           WRITE REPORT-LINE FROM RPT-DETAIL-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE 'Medium Risk Policies (120-179):' TO RPT-DETAIL-LABEL
           MOVE WS-MED-RISK-CNT TO RPT-DETAIL-VALUE
           WRITE REPORT-LINE FROM RPT-DETAIL-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE 'Low Risk Policies (<120):' TO RPT-DETAIL-LABEL
           MOVE WS-LOW-RISK-CNT TO RPT-DETAIL-VALUE
           WRITE REPORT-LINE FROM RPT-DETAIL-LINE 
               AFTER ADVANCING 1 LINE
           
           WRITE REPORT-LINE FROM RPT-BLANK-LINE 
               AFTER ADVANCING 2 LINES
           MOVE 'END OF REPORT' TO RPT-SECTION-TITLE
           WRITE REPORT-LINE FROM RPT-SECTION-HEADER 
               AFTER ADVANCING 1 LINE.

       P700-CLOSE-FILES.
           CLOSE INPUT-FILE
           CLOSE REPORT-FILE.