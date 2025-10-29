       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGCOMCAL.
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
                                        VALUE 'LGCOMCAL------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-FILLER                PIC X.
           03 WS-ADDR-COMMAREA         USAGE is POINTER.
           03 WS-CALEN                 PIC S9(4) COMP.

      * Variables for time/date processing
       01  ABS-TIME                    PIC S9(8) COMP VALUE +0.
       01  TIME1                       PIC X(8)  VALUE SPACES.
       01  DATE1                       PIC X(10) VALUE SPACES.

      * Error Message structure
       01  ERROR-MSG.
           03 EM-DATE                  PIC X(8)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-TIME                  PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X(9)  VALUE ' LGCOMCAL'.
           03 EM-VARIABLE              PIC X(61) VALUE SPACES.

      * Various processing flags and accumulators
       01  WS-VARIABLES.
           03 WS-SYSTEM-DATE-CYMD      PIC 9(8).
           03 FILLER REDEFINES WS-SYSTEM-DATE-CYMD.
              05 WS-SYS-YEAR-CY        PIC 9(4).
              05 WS-SYS-MONTH-MM       PIC 9(2).
              05 WS-SYS-DAY-DD         PIC 9(2).
           03 WS-PROCESSING-FLAGS.
              05 WS-SEC-CHECK-OK       PIC X VALUE 'N'.
              05 WS-MATRIX-ENABLED     PIC X VALUE 'Y'.
              05 WS-T24-CHECK          PIC X VALUE 'Y'.
           03 WS-ACCUMULATORS.
              05 WS-SUB-1              PIC S9(4) COMP VALUE 0.
              05 WS-SUB-2              PIC S9(4) COMP VALUE 0.
              05 WS-CALC-SUB           PIC S9(4) COMP VALUE 0.
              05 WS-CNT-1              PIC S9(4) COMP VALUE 0.
              05 WS-XFACTOR            PIC S9(8) COMP VALUE 0.
              05 WS-TEMP-CALC          PIC S9(8) COMP VALUE 0.
              05 WS-TEMP-SCORE         PIC S9(8) COMP VALUE 0.

       01  WS-PROP-MATRIX.
           03 WS-PROP-VALUES.
              05 FILLER                PIC X(25) VALUE 'WAREHOUSE       00050'.
              05 FILLER                PIC X(25) VALUE 'FACTORY         00075'.
              05 FILLER                PIC X(25) VALUE 'OFFICE          00025'.
              05 FILLER                PIC X(25) VALUE 'RETAIL          00040'.
              05 FILLER                PIC X(25) VALUE 'MIXEDUSE        00045'.
              05 FILLER                PIC X(25) VALUE 'COMMERCIAL      00035'.
              05 FILLER                PIC X(25) VALUE 'INDUSTRIAL      00070'.
              05 FILLER                PIC X(25) VALUE 'MEDICAL         00030'.
           03 WS-PROP-TABLE REDEFINES WS-PROP-VALUES.
              05 WS-PROP-ENTRY OCCURS 8 TIMES.
                 07 WS-PROP-NAME       PIC X(15).
                 07 WS-PROP-FACTOR     PIC 9(5).

       01  WS-GEO-MATRIX.
           03 WS-GEO-VALUES.
              05 FILLER                PIC X(10) VALUE 'FL0000030'.
              05 FILLER                PIC X(10) VALUE 'CR0000030'.
              05 FILLER                PIC X(10) VALUE 'NY0000020'.
              05 FILLER                PIC X(10) VALUE 'CA0000025'.
              05 FILLER                PIC X(10) VALUE 'TX0000015'.
              05 FILLER                PIC X(10) VALUE 'AZ0000010'.
           03 WS-GEO-TABLE REDEFINES WS-GEO-VALUES.
              05 WS-GEO-ENTRY OCCURS 6 TIMES.
                 07 WS-GEO-CODE        PIC XX.
                 07 WS-GEO-FACTOR      PIC 9(7).

       01  WS-PERIL-FACTORS.
           03 WS-PERIL-VALUES.
              05 FILLER                PIC X(15) VALUE 'FIRE      00080'.
              05 FILLER                PIC X(15) VALUE 'CRIME     00060'.
              05 FILLER                PIC X(15) VALUE 'FLOOD     00120'.
              05 FILLER                PIC X(15) VALUE 'WEATHER   00090'.
           03 WS-PERIL-TABLE REDEFINES WS-PERIL-VALUES.
              05 WS-PERIL-ENTRY OCCURS 4 TIMES.
                 07 WS-PERIL-NAME      PIC X(10).
                 07 WS-PERIL-FACTOR    PIC 9(5).

       01  WS-REV-MAPPING.
           03 WS-RM-PROP               PIC 9 VALUE 0.
           03 WS-RM-PERIL              PIC 9 VALUE 2.
           03 WS-SCALE-FACTORS.
              05 WS-PERIL-INDEXER      PIC 9(4).
              05 WS-PROP-INDEXER       PIC 9(4).
           03 WS-MATRIX-SELECTORS.
              05 WS-MS-SIZE            PIC 9(4) VALUE 3.
              05 WS-MS-OFFSET          PIC 9(4) VALUE 2.
              05 WS-MS-SCALE           PIC 9(4) VALUE 1.

       01  WS-SECURITY-MAPPINGS.
           03 WS-SEC-OFFSETS.
              05 WS-SEC-IDX-1          PIC 9(4) VALUE 1.
              05 WS-SEC-IDX-2          PIC 9(4) VALUE 2.
              05 WS-SEC-IDX-3          PIC 9(4) VALUE 3.
              05 WS-SEC-IDX-4          PIC 9(4) VALUE 4.
           03 WS-SEC-ENABLED           PIC X VALUE 'Y'.
              
       01  WS-TRANSFORM-MATRIX.
           03 WS-TM-BASE               PIC 9(3) VALUE 100.
           03 WS-TM-GEO-FACTORS.
              05 WS-TM-GF-1            PIC 9(3) VALUE 100.
              05 WS-TM-GF-2            PIC 9(3) VALUE 130.
              05 WS-TM-GF-3            PIC 9(3) VALUE 115.
           03 WS-TM-STATUS-LOOKUP.
              05 WS-TM-SL-1            PIC 9(3) VALUE 150.
              05 WS-TM-SL-2            PIC 9(3) VALUE 200.
              05 WS-TM-SL-3            PIC 9(3) VALUE 250.
      
       01  WS-SECRET-AREA.
           03 WS-SA-MULT               PIC V99 VALUE 1.00.
           03 WS-SA-DISCOUNT           PIC V99 VALUE 0.90.
           03 WS-SA-FIRE-FACTOR        PIC V99 VALUE 0.80.
           03 WS-SA-CRIME-FACTOR       PIC V99 VALUE 0.60.
           03 WS-SA-FLOOD-FACTOR       PIC V99 VALUE 1.20.
           03 WS-SA-WEATHER-FACTOR     PIC V99 VALUE 0.90.
           03 WS-SA-BASE-RISK          PIC 999 VALUE 100.
           03 WS-SA-RISK               PIC 999 VALUE 0.
           03 WS-SA-WAREHOUSE-FACTOR   PIC 999 VALUE 50.
           03 WS-SA-FACTORY-FACTOR     PIC 999 VALUE 75.
           03 WS-SA-OFFICE-FACTOR      PIC 999 VALUE 25.
           03 WS-SA-RETAIL-FACTOR      PIC 999 VALUE 40.
           03 WS-SA-POSTCODE-FACTOR    PIC 999 VALUE 30.
           03 WS-SA-STAT-MED-THRESHOLD PIC 999 VALUE 150.
           03 WS-SA-STAT-HIGH-THRESHOLD PIC 999 VALUE 200.

       01  WS-RISK-CALC.
           03 WS-RC-BASE-VAL           PIC 9(3) VALUE 0.
           03 WS-RC-PROP-FACT          PIC 9(3) VALUE 0.
           03 WS-RC-GEO-FACT           PIC 9(3) VALUE 0.
           03 WS-RC-TOTAL              PIC 9(3) VALUE 0.
           03 WS-RC-STATUS             PIC 9 VALUE 0.
           03 WS-RC-REASON             PIC X(50) VALUE SPACES.
           03 WS-RC-PREMIUMS.
              05 WS-RC-PREM-FIRE       PIC 9(8) VALUE 0.
              05 WS-RC-PREM-CRIME      PIC 9(8) VALUE 0.
              05 WS-RC-PREM-FLOOD      PIC 9(8) VALUE 0.
              05 WS-RC-PREM-WEATHER    PIC 9(8) VALUE 0.
              05 WS-RC-DISCOUNT        PIC V99 VALUE 1.00.
        
       01  WS-LGSCMTRX                 PIC X(8) VALUE 'LGSCMTRX'.
       01  WS-PRISCAL                  PIC X(8) VALUE 'LGPCALC1'.
       01  WS-COMP-AREA.
           03 WS-COMP-ACTION           PIC X VALUE SPACES.
           03 WS-COMP-DATA             PIC X(299) VALUE SPACES.
           03 WS-COMP-RESULT           PIC X(100) VALUE SPACES.
           
       COPY LGCOMDAT.
            
      ******************************************************************
      *    L I N K A G E     S E C T I O N
      ******************************************************************
       LINKAGE SECTION.

       01  DFHCOMMAREA.
           03  CA-XCUSTID             PIC X(10).
           03  CA-XPOLNUM             PIC X(10).
           03  CA-XPROPTYPE           PIC X(15).
           03  CA-XPOSTCODE           PIC X(8).
           03  CA-XFP-FACTOR          PIC 9(4).
           03  CA-XCP-FACTOR          PIC 9(4).
           03  CA-XFLP-FACTOR         PIC 9(4).
           03  CA-XWP-FACTOR          PIC 9(4).
           03  CA-XADDRESS            PIC X(255).
           03  CA-XLAT                PIC X(11).
           03  CA-XLONG               PIC X(11).
           03  CA-XCUSTNAME           PIC X(31).
           03  CA-XISSUE              PIC X(10).
           03  CA-XEXPIRY             PIC X(10).
           03  CA-XLASTCHG            PIC X(26).
           03  CA-XCALC-MATRIX.
               05  CA-XM-TYPE         PIC X.
               05  CA-XM-FACTORS      OCCURS 10 TIMES.
                   07  CA-XMF-CODE    PIC XX.
                   07  CA-XMF-VALUE   PIC S9(3)V99.
           03  CA-ZRESULT-SCORE       PIC 999.
           03  CA-ZSTATUS-IND         PIC 9.
           03  CA-ZREJECT-TEXT        PIC X(50).
           03  CA-ZFP-PREMIUM         PIC 9(8).
           03  CA-ZCP-PREMIUM         PIC 9(8).
           03  CA-ZFLP-PREMIUM        PIC 9(8).
           03  CA-ZWP-PREMIUM         PIC 9(8).
           03  CA-ZRISK-FACTORS       PIC X(100).

      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
       MAINLINE SECTION.
           
           PERFORM INITIALIZE-PROCESSING.
           PERFORM PROCESS-BUSINESS-LOGIC.
           PERFORM CLEANUP-AND-EXIT.
           
       MAINLINE-EXIT.
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       INITIALIZE-PROCESSING.
           INITIALIZE WS-HEADER.
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.
           
           PERFORM INITIALIZE-MATRICES.
           
           INITIALIZE WS-RISK-CALC.
           
           PERFORM INIT-SECURITY-VALIDATION.
           
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       INITIALIZE-MATRICES.
           MOVE 'Y' TO WS-SEC-ENABLED.
           MOVE 1 TO WS-SUB-1.
           
           PERFORM VARYING WS-SUB-1 FROM 1 BY 1 
             UNTIL WS-SUB-1 > 5
               MOVE 0 TO WS-SUB-2
               PERFORM VARYING WS-SUB-2 FROM 1 BY 1 
                 UNTIL WS-SUB-2 > 6
                   IF WS-SUB-1 = 3 AND WS-SUB-2 = 2
                      MOVE 1 TO WS-RM-PROP
                   END-IF
                   IF WS-SUB-1 = 2 AND WS-SUB-2 = 3
                      MOVE 3 TO WS-RM-PERIL
                   END-IF
               END-PERFORM
           END-PERFORM.
           
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       INIT-SECURITY-VALIDATION.
           MOVE 'Y' TO WS-SEC-CHECK-OK.
           MOVE 'Y' TO WS-T24-CHECK.
           
           MOVE 1 TO WS-SEC-IDX-1.
           MOVE 2 TO WS-SEC-IDX-2.
           MOVE 4 TO WS-SEC-IDX-4.
           MOVE 3 TO WS-SEC-IDX-3.
           
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       PROCESS-BUSINESS-LOGIC.
           PERFORM PROCESS-RISK-SCORE.
           PERFORM DETERMINE-POLICY-STATUS.
           PERFORM CALCULATE-PREMIUMS.
           
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       PROCESS-RISK-SCORE.
           MOVE WS-TM-BASE TO WS-TEMP-SCORE.
           DIVIDE 2 INTO WS-TEMP-SCORE GIVING WS-SUB-1.
           MULTIPLY 2 BY WS-SUB-1 GIVING WS-RC-BASE-VAL.
           
           MOVE 0 TO WS-RC-PROP-FACT.
           
           MOVE 'COMMERCIAL' TO RMS-TYPE
           MOVE '1.0.5' TO RMS-VERSION
      
           EVALUATE CA-XPROPTYPE
               WHEN 'WAREHOUSE'
                   MOVE RMS-PF-W-VAL TO RMS-PF-WAREHOUSE
                   COMPUTE WS-TEMP-CALC = RMS-PF-WAREHOUSE
                   ADD WS-TEMP-CALC TO WS-RC-PROP-FACT
               WHEN 'FACTORY'
                   MOVE RMS-PF-F-VAL TO RMS-PF-FACTORY
                   COMPUTE WS-TEMP-CALC = RMS-PF-FACTORY
                   ADD WS-TEMP-CALC TO WS-RC-PROP-FACT
               WHEN 'OFFICE'
                   MOVE RMS-PF-O-VAL TO RMS-PF-OFFICE
                   COMPUTE WS-TEMP-CALC = RMS-PF-OFFICE
                   ADD WS-TEMP-CALC TO WS-RC-PROP-FACT
               WHEN 'RETAIL'
                   MOVE RMS-PF-R-VAL TO RMS-PF-RETAIL
                   COMPUTE WS-TEMP-CALC = RMS-PF-RETAIL
                   ADD WS-TEMP-CALC TO WS-RC-PROP-FACT
               WHEN OTHER
                   MOVE 0 TO WS-RC-PROP-FACT
           END-EVALUATE.
           
           MOVE 0 TO WS-RC-GEO-FACT.
           
           MOVE RMS-GF-FL-VAL TO RMS-GF-FL
           MOVE RMS-GF-CR-VAL TO RMS-GF-CR
           
           IF CA-XPOSTCODE(1:2) = 'FL'
              MOVE RMS-GF-FL TO WS-RC-GEO-FACT
           ELSE
              IF CA-XPOSTCODE(1:2) = 'CR'
                 MOVE RMS-GF-CR TO WS-RC-GEO-FACT
              END-IF
           END-IF.
           
           COMPUTE WS-RC-TOTAL = 
              WS-RC-BASE-VAL + WS-RC-PROP-FACT + WS-RC-GEO-FACT.
              
           MOVE WS-RC-TOTAL TO WS-SA-RISK.
           
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       DETERMINE-POLICY-STATUS.
           MOVE 0 TO WS-RC-STATUS.
           MOVE SPACES TO WS-RC-REASON.
           
           MOVE RMS-TH-L1-VAL TO RMS-TH-LEVEL-1
           MOVE RMS-TH-L2-VAL TO RMS-TH-LEVEL-2
           
           IF WS-SA-RISK > RMS-TH-LEVEL-2
              MOVE 2 TO WS-RC-STATUS
              MOVE 'High Risk Score - Manual Review Required' 
                TO WS-RC-REASON
              MOVE WS-SA-RISK TO CID-FINAL-SCORE
              MOVE WS-RC-STATUS TO CID-STATUS
              MOVE WS-RC-REASON TO CID-REASON
           ELSE
              IF WS-SA-RISK > RMS-TH-LEVEL-1
                 MOVE 1 TO WS-RC-STATUS
                 MOVE 'Medium Risk - Pending Review'
                   TO WS-RC-REASON
                 MOVE WS-SA-RISK TO CID-FINAL-SCORE
                 MOVE WS-RC-STATUS TO CID-STATUS
                 MOVE WS-RC-REASON TO CID-REASON
              ELSE
                 MOVE 0 TO WS-RC-STATUS
                 MOVE SPACES TO WS-RC-REASON
                 MOVE WS-SA-RISK TO CID-FINAL-SCORE
                 MOVE WS-RC-STATUS TO CID-STATUS
                 MOVE SPACES TO CID-REASON
              END-IF
           END-IF.
           
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       CALCULATE-PREMIUMS.
           MOVE 1.00 TO WS-RC-DISCOUNT.
           
           MOVE RMS-PERF-F-VAL TO RMS-PERF-FIRE
           MOVE RMS-PERF-C-VAL TO RMS-PERF-CRIME
           MOVE RMS-PERF-FL-VAL TO RMS-PERF-FLOOD
           MOVE RMS-PERF-W-VAL TO RMS-PERF-WEATHER
           
           IF CA-XFP-FACTOR > 0 AND
              CA-XCP-FACTOR > 0 AND
              CA-XFLP-FACTOR > 0 AND
              CA-XWP-FACTOR > 0
              MOVE RMS-DISCOUNT-FACTOR TO WS-RC-DISCOUNT
              MOVE RMS-DISCOUNT-FACTOR TO CID-DISCOUNT-PCT
           END-IF.
           
           PERFORM COMPLEX-PREMIUM-CALCULATIONS.
           
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       COMPLEX-PREMIUM-CALCULATIONS.      
           IF CA-XFP-FACTOR > 0
              COMPUTE WS-TEMP-CALC = 
                 WS-SA-RISK * RMS-PERF-FIRE
              COMPUTE WS-RC-PREM-FIRE =
                 (WS-TEMP-CALC * CA-XFP-FACTOR * WS-RC-DISCOUNT)
              MOVE WS-RC-PREM-FIRE TO CID-FIRE-PREMIUM
           ELSE
              MOVE 0 TO WS-RC-PREM-FIRE
              MOVE 0 TO CID-FIRE-PREMIUM
           END-IF.
           
           IF CA-XCP-FACTOR > 0
              COMPUTE WS-TEMP-CALC = 
                 WS-SA-RISK * RMS-PERF-CRIME
              COMPUTE WS-RC-PREM-CRIME =
                 (WS-TEMP-CALC * CA-XCP-FACTOR * WS-RC-DISCOUNT)
              MOVE WS-RC-PREM-CRIME TO CID-CRIME-PREMIUM
           ELSE
              MOVE 0 TO WS-RC-PREM-CRIME
              MOVE 0 TO CID-CRIME-PREMIUM
           END-IF.
           
           IF CA-XFLP-FACTOR > 0
              COMPUTE WS-TEMP-CALC = 
                 WS-SA-RISK * RMS-PERF-FLOOD
              COMPUTE WS-RC-PREM-FLOOD =
                 (WS-TEMP-CALC * CA-XFLP-FACTOR * WS-RC-DISCOUNT)
              MOVE WS-RC-PREM-FLOOD TO CID-FLOOD-PREMIUM
           ELSE
              MOVE 0 TO WS-RC-PREM-FLOOD
              MOVE 0 TO CID-FLOOD-PREMIUM
           END-IF.
           
           IF CA-XWP-FACTOR > 0
              COMPUTE WS-TEMP-CALC = 
                 WS-SA-RISK * RMS-PERF-WEATHER
              COMPUTE WS-RC-PREM-WEATHER =
                 (WS-TEMP-CALC * CA-XWP-FACTOR * WS-RC-DISCOUNT)
              MOVE WS-RC-PREM-WEATHER TO CID-WEATHER-PREMIUM
           ELSE
              MOVE 0 TO WS-RC-PREM-WEATHER
              MOVE 0 TO CID-WEATHER-PREMIUM
           END-IF.
           
           COMPUTE CID-TOTAL-PREMIUM =
              CID-FIRE-PREMIUM + CID-CRIME-PREMIUM +
              CID-FLOOD-PREMIUM + CID-WEATHER-PREMIUM.
              
           EXIT.
      *----------------------------------------------------------------*
           
      *----------------------------------------------------------------*
       CLEANUP-AND-EXIT.
           MOVE WS-SA-RISK TO CA-ZRESULT-SCORE.
           MOVE WS-RC-STATUS TO CA-ZSTATUS-IND.
           MOVE WS-RC-REASON TO CA-ZREJECT-TEXT.
           MOVE WS-RC-PREM-FIRE TO CA-ZFP-PREMIUM.
           MOVE WS-RC-PREM-CRIME TO CA-ZCP-PREMIUM.
           MOVE WS-RC-PREM-FLOOD TO CA-ZFLP-PREMIUM.
           MOVE WS-RC-PREM-WEATHER TO CA-ZWP-PREMIUM.
           
           EXEC CICS RETURN END-EXEC.
           
           EXIT.
      *----------------------------------------------------------------* 