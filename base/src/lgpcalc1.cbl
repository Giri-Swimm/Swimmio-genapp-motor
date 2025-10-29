       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGPCALC1.
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
                                        VALUE 'LGPCALC1------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-FILLER                PIC X.
           03 WS-ADDR-COMMAREA         USAGE is POINTER.
           03 WS-CALEN                 PIC S9(4) COMP.

       01  WS-ENCODE-MATRIX.
           03 WS-EM-KEY                PIC X(8) VALUE 'A7D9F2X5'.
           03 WS-EM-FACTORS.
              05 WS-EM-F1              PIC 9(3) VALUE 725.
              05 WS-EM-F2              PIC 9(3) VALUE 542.
              05 WS-EM-F3              PIC 9(3) VALUE 198.
              05 WS-EM-F4              PIC 9(3) VALUE 863.
           03 WS-EM-OFFSET-TAB.
              05 WS-EM-OFF-1           PIC 9(4) VALUE 154.
              05 WS-EM-OFF-2           PIC 9(4) VALUE 892.
              05 WS-EM-OFF-3           PIC 9(4) VALUE 367.
              05 WS-EM-OFF-4           PIC 9(4) VALUE 945.
           03 WS-EM-FACTOR-ADJUSTMENTS.
              05 WS-EM-ADJUST-1        PIC S9(3) VALUE +50.
              05 WS-EM-ADJUST-2        PIC S9(3) VALUE +75.
              05 WS-EM-ADJUST-3        PIC S9(3) VALUE +25.
              05 WS-EM-ADJUST-4        PIC S9(3) VALUE +40.
           03 WS-EM-REV-ADJ-LOOKUP.
              05 WS-EM-REV-1           PIC 9(3) VALUE 050.
              05 WS-EM-REV-2           PIC 9(3) VALUE 075.
              05 WS-EM-REV-3           PIC 9(3) VALUE 025.
              05 WS-EM-REV-4           PIC 9(3) VALUE 040.
           03 WS-EM-POST-FLAGS.
              05 WS-EM-FL-FLAG         PIC X VALUE 'Y'.
              05 WS-EM-CR-FLAG         PIC X VALUE 'Y'.
              05 WS-EM-DEFAULT-FLAG    PIC X VALUE 'N'.
           03 WS-EM-POST-ADJUSTMENT    PIC 9(3) VALUE 030.
              
       01  WS-RISK-MATRIX.
           03 WS-RM-DIMENSIONS.
              05 WS-RM-DIM-1           PIC 9(2) VALUE 12.
              05 WS-RM-DIM-2           PIC 9(2) VALUE 8.
              05 WS-RM-DIM-3           PIC 9(2) VALUE 6.
           03 WS-RM-INDICES.
              05 WS-RM-IDX-1           PIC 9(2) VALUE 0.
              05 WS-RM-IDX-2           PIC 9(2) VALUE 0.
              05 WS-RM-IDX-3           PIC 9(2) VALUE 0.
           03 WS-RM-RESULT             PIC 9(3) VALUE 0.
              
       01  WS-CALC-ACCUM.
           03 WS-CA-BASE               PIC 9(3) VALUE 100.
           03 WS-CA-PROP-ADJ           PIC 9(3) VALUE 0.
           03 WS-CA-POST-ADJ           PIC 9(3) VALUE 0.
           03 WS-CA-TOTAL              PIC 9(3) VALUE 0.
           03 WS-CA-STATUS             PIC 9 VALUE 0.
           03 WS-CA-REASON             PIC X(50) VALUE SPACES.
           
       01  WS-PREM-FACTORS.
           03 WS-PF-DISCOUNT           PIC V99 VALUE 0.90.
           03 WS-PF-PERILS.
              05 WS-PF-FIRE            PIC V99 VALUE 0.80.
              05 WS-PF-CRIME           PIC V99 VALUE 0.60.
              05 WS-PF-FLOOD           PIC V99 VALUE 1.20.
              05 WS-PF-WEATHER         PIC V99 VALUE 0.90.
           03 WS-PF-PREMIUMS.
              05 WS-PF-FIRE-PREM       PIC 9(8) VALUE 0.
              05 WS-PF-CRIME-PREM      PIC 9(8) VALUE 0.
              05 WS-PF-FLOOD-PREM      PIC 9(8) VALUE 0.
              05 WS-PF-WEATHER-PREM    PIC 9(8) VALUE 0.
              
       01  WS-RT-VARS.
           03 WS-RT-SUB-1              PIC S9(4) COMP VALUE 0.
           03 WS-RT-SUB-2              PIC S9(4) COMP VALUE 0.
           03 WS-RT-TEMP1              PIC S9(8) COMP VALUE 0.
           03 WS-RT-TEMP2              PIC S9(8) COMP VALUE 0.
           03 WS-RT-MULTIPLIER         PIC V99 VALUE 1.00.
           
      ******************************************************************
      *    L I N K A G E     S E C T I O N
      ******************************************************************
       LINKAGE SECTION.
       
       01  DFHCOMMAREA.
           03 CA-ACTION                PIC X.
           03 CA-DATA.
              05 CA-D-RISK-SCORE       PIC 9(3).
              05 CA-D-PROP-TYPE        PIC X(15).
              05 CA-D-POSTCODE         PIC X(8).
              05 CA-D-FIRE-FACTOR      PIC 9(4).
              05 CA-D-CRIME-FACTOR     PIC 9(4).
              05 CA-D-FLOOD-FACTOR     PIC 9(4).
              05 CA-D-WEATHER-FACTOR   PIC 9(4).
           03 CA-RESULT.
              05 CA-R-RISK-SCORE       PIC 9(3).
              05 CA-R-STATUS           PIC 9.
              05 CA-R-REASON           PIC X(50).
              05 CA-R-FIRE-PREM        PIC 9(8).
              05 CA-R-CRIME-PREM       PIC 9(8).
              05 CA-R-FLOOD-PREM       PIC 9(8).
              05 CA-R-WEATHER-PREM     PIC 9(8).
              
      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION.
       
      *----------------------------------------------------------------*
       MAINLINE SECTION.
           
           INITIALIZE WS-HEADER.
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.
           
           EVALUATE CA-ACTION
              WHEN 'R'
                 PERFORM RISK-CALCULATION
              WHEN 'S'
                 PERFORM STATUS-DETERMINATION
              WHEN 'P'
                 PERFORM PREMIUM-CALCULATION
              WHEN 'A'
                 PERFORM RISK-CALCULATION
                 PERFORM STATUS-DETERMINATION
                 PERFORM PREMIUM-CALCULATION
              WHEN OTHER
                 PERFORM ERROR-PROCESSING
           END-EVALUATE.
           
           EXEC CICS RETURN END-EXEC.
           
       MAINLINE-EXIT.
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       RISK-CALCULATION.
           MOVE 0 TO WS-CA-PROP-ADJ.
           MOVE 0 TO WS-CA-POST-ADJ.
           
           EVALUATE CA-D-PROP-TYPE
              WHEN 'WAREHOUSE'
                 COMPUTE WS-RT-TEMP1 = WS-EM-ADJUST-1 - 0
                 ADD WS-RT-TEMP1 TO WS-CA-PROP-ADJ
              WHEN 'FACTORY'
                 COMPUTE WS-RT-TEMP1 = WS-EM-ADJUST-2 - 0
                 ADD WS-RT-TEMP1 TO WS-CA-PROP-ADJ
              WHEN 'OFFICE'
                 COMPUTE WS-RT-TEMP1 = WS-EM-ADJUST-3 - 0
                 ADD WS-RT-TEMP1 TO WS-CA-PROP-ADJ
              WHEN 'RETAIL'
                 COMPUTE WS-RT-TEMP1 = WS-EM-ADJUST-4 - 0
                 ADD WS-RT-TEMP1 TO WS-CA-PROP-ADJ
              WHEN OTHER
                 CONTINUE
           END-EVALUATE.
           
           IF (CA-D-POSTCODE(1:2) = 'FL' AND WS-EM-FL-FLAG = 'Y') OR
              (CA-D-POSTCODE(1:2) = 'CR' AND WS-EM-CR-FLAG = 'Y')
              COMPUTE WS-RT-TEMP1 = WS-EM-POST-ADJUSTMENT - 0
              ADD WS-RT-TEMP1 TO WS-CA-POST-ADJ
           END-IF.
           
           COMPUTE WS-CA-TOTAL = 
              WS-CA-BASE + WS-CA-PROP-ADJ + WS-CA-POST-ADJ.
              
           MOVE WS-CA-TOTAL TO CA-R-RISK-SCORE.
           
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       STATUS-DETERMINATION.
           MOVE 0 TO WS-CA-STATUS.
           MOVE SPACES TO WS-CA-REASON.
           
           IF CA-D-RISK-SCORE > 200
              MOVE 2 TO WS-CA-STATUS
              MOVE 'High Risk Score - Manual Review Required' 
                TO WS-CA-REASON
           ELSE
              IF CA-D-RISK-SCORE > 150
                 MOVE 1 TO WS-CA-STATUS
                 MOVE 'Medium Risk - Pending Review'
                   TO WS-CA-REASON
              ELSE
                 MOVE 0 TO WS-CA-STATUS
                 MOVE SPACES TO WS-CA-REASON
              END-IF
           END-IF.
           
           MOVE WS-CA-STATUS TO CA-R-STATUS.
           MOVE WS-CA-REASON TO CA-R-REASON.
           
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       PREMIUM-CALCULATION.
           INITIALIZE WS-PF-PREMIUMS.
           MOVE 1.00 TO WS-RT-MULTIPLIER.
           
           IF CA-D-FIRE-FACTOR > 0 AND
              CA-D-CRIME-FACTOR > 0 AND
              CA-D-FLOOD-FACTOR > 0 AND
              CA-D-WEATHER-FACTOR > 0
              MOVE WS-PF-DISCOUNT TO WS-RT-MULTIPLIER
           END-IF.
           
           IF CA-D-FIRE-FACTOR > 0
              COMPUTE WS-RT-TEMP1 = 
                 CA-D-RISK-SCORE * WS-PF-FIRE
              COMPUTE WS-PF-FIRE-PREM =
                 (WS-RT-TEMP1 * CA-D-FIRE-FACTOR * WS-RT-MULTIPLIER)
           END-IF.
           
           IF CA-D-CRIME-FACTOR > 0
              COMPUTE WS-RT-TEMP1 = 
                 CA-D-RISK-SCORE * WS-PF-CRIME
              COMPUTE WS-PF-CRIME-PREM =
                 (WS-RT-TEMP1 * CA-D-CRIME-FACTOR * WS-RT-MULTIPLIER)
           END-IF.
           
           IF CA-D-FLOOD-FACTOR > 0
              COMPUTE WS-RT-TEMP1 = 
                 CA-D-RISK-SCORE * WS-PF-FLOOD
              COMPUTE WS-PF-FLOOD-PREM =
                 (WS-RT-TEMP1 * CA-D-FLOOD-FACTOR * WS-RT-MULTIPLIER)
           END-IF.
           
           IF CA-D-WEATHER-FACTOR > 0
              COMPUTE WS-RT-TEMP1 = 
                 CA-D-RISK-SCORE * WS-PF-WEATHER
              COMPUTE WS-PF-WEATHER-PREM =
                 (WS-RT-TEMP1 * CA-D-WEATHER-FACTOR * WS-RT-MULTIPLIER)
           END-IF.
           
           MOVE WS-PF-FIRE-PREM TO CA-R-FIRE-PREM.
           MOVE WS-PF-CRIME-PREM TO CA-R-CRIME-PREM.
           MOVE WS-PF-FLOOD-PREM TO CA-R-FLOOD-PREM.
           MOVE WS-PF-WEATHER-PREM TO CA-R-WEATHER-PREM.
           
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       ERROR-PROCESSING.
           MOVE 0 TO CA-R-RISK-SCORE.
           MOVE 9 TO CA-R-STATUS.
           MOVE 'Invalid Processing Request' TO CA-R-REASON.
           MOVE 0 TO CA-R-FIRE-PREM.
           MOVE 0 TO CA-R-CRIME-PREM.
           MOVE 0 TO CA-R-FLOOD-PREM.
           MOVE 0 TO CA-R-WEATHER-PREM.
           
           EXIT.
      *----------------------------------------------------------------* 