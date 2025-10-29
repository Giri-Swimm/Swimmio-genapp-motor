      ******************************************************************
      * Commercial Insurance Data Structures
      ******************************************************************
       01  COMM-INSURANCE-DATA.
           03  CID-REQ-TYPE             PIC X.
           03  CID-TRACKING-DATA.
               05  CID-ORG-ID           PIC X(5).
               05  CID-REQUEST-DATE     PIC X(10).
               05  CID-REQ-TIME         PIC X(8).
               05  CID-AUTH-LEVEL       PIC 9.
               05  CID-CUST-GROUP       PIC X(5).
           03  CID-RISK-ELEMENTS.
               05  CID-ASSET-DATA.
                   07  CID-ASSET-TYPE   PIC X(12).
                   07  CID-LOCATION     PIC X(255).
                   07  CID-POSTCODE     PIC X(8).
                   07  CID-GEOLOC.
                       09  CID-LAT      PIC X(11).
                       09  CID-LONG     PIC X(11).
                   07  CID-VALUATION    PIC 9(8).
                   07  CID-AGE          PIC 9(3).
                   07  CID-CONSTRUCTION PIC X(15).
               05  CID-PERIL-DATA.
                   07  CID-PERIL-FLAGS.
                       09  CID-FIRE-FLAG    PIC X.
                       09  CID-CRIME-FLAG   PIC X.
                       09  CID-FLOOD-FLAG   PIC X.
                       09  CID-WEATHER-FLAG PIC X.
                   07  CID-PERIL-FACTORS.
                       09  CID-FIRE-FACTOR     PIC 9(4).
                       09  CID-CRIME-FACTOR    PIC 9(4).
                       09  CID-FLOOD-FACTOR    PIC 9(4).
                       09  CID-WEATHER-FACTOR  PIC 9(4).
           03  CID-WEIGHT-FACTORS.
               05  CID-W-ASSET          PIC V999 VALUE 0.333.
               05  CID-W-LOCATION       PIC V999 VALUE 0.333.
               05  CID-W-DEMOGRAPHIC    PIC V999 VALUE 0.334.
           03  CID-MATRIX-ELEMENTS.
               05  CID-ME-TYPE          PIC X.
               05  CID-ME-VERSION       PIC 9(3).
               05  CID-ME-WEIGHTS       OCCURS 5 TIMES INDEXED BY CID-IDX.
                   07  CID-ME-CODE      PIC XX.
                   07  CID-ME-VALUE     PIC S9(3)V99.
           03  CID-ANALYTICS-DATA.
               05  CID-STAT-MODEL       PIC X(10).
               05  CID-STAT-WEIGHTS     PIC X(100).
               05  CID-RISK-FACTORS     PIC X(100).
           03  CID-RESULT-DATA.
               05  CID-BASE-SCORE       PIC 9(3).
               05  CID-ADJ-SCORE        PIC 9(3).
               05  CID-FINAL-SCORE      PIC 9(3).
               05  CID-STATUS           PIC 9.
               05  CID-REASON           PIC X(50).
               05  CID-PRICING-DATA.
                   07  CID-BASE-PREMIUM PIC 9(6)V99.
                   07  CID-FIRE-PREMIUM PIC 9(6)V99.
                   07  CID-CRIME-PREMIUM PIC 9(6)V99.
                   07  CID-FLOOD-PREMIUM PIC 9(6)V99.
                   07  CID-WEATHER-PREMIUM PIC 9(6)V99.
                   07  CID-DISCOUNT-PCT PIC V99.
                   07  CID-TOTAL-PREMIUM PIC 9(8)V99.
                   
      ******************************************************************
      * Risk Assessment Matrix Structure
      ******************************************************************
       01  RISK-MATRIX-STRUCTURE.
           03  RMS-TYPE                 PIC X(10).
           03  RMS-VERSION              PIC X(5).
           03  RMS-BASE                 PIC 9(3).
           03  RMS-PROP-FACTORS.
               05  RMS-PF-ENCODED.
                   07  RMS-PF-WAREHOUSE PIC 9(3).
                   07  RMS-PF-FACTORY   PIC 9(3).
                   07  RMS-PF-OFFICE    PIC 9(3).
                   07  RMS-PF-RETAIL    PIC 9(3).
               05  RMS-PF-DECODED.
                   07  RMS-PF-W-VAL     PIC 9(3) VALUE 050.
                   07  RMS-PF-F-VAL     PIC 9(3) VALUE 075.
                   07  RMS-PF-O-VAL     PIC 9(3) VALUE 025.
                   07  RMS-PF-R-VAL     PIC 9(3) VALUE 040.
           03  RMS-GEO-FACTORS.
               05  RMS-GF-ENCODED.
                   07  RMS-GF-FL        PIC 9(3).
                   07  RMS-GF-CR        PIC 9(3).
               05  RMS-GF-DECODED.
                   07  RMS-GF-FL-VAL    PIC 9(3) VALUE 030.
                   07  RMS-GF-CR-VAL    PIC 9(3) VALUE 030.
           03  RMS-THRESHOLDS.
               05  RMS-TH-ENCODED.
                   07  RMS-TH-LEVEL-1   PIC 9(3).
                   07  RMS-TH-LEVEL-2   PIC 9(3).
               05  RMS-TH-DECODED.
                   07  RMS-TH-L1-VAL    PIC 9(3) VALUE 150.
                   07  RMS-TH-L2-VAL    PIC 9(3) VALUE 200.
           03  RMS-PERIL-FACTORS.
               05  RMS-PERF-ENCODED.
                   07  RMS-PERF-FIRE    PIC V99.
                   07  RMS-PERF-CRIME   PIC V99.
                   07  RMS-PERF-FLOOD   PIC V99.
                   07  RMS-PERF-WEATHER PIC V99.
               05  RMS-PERF-DECODED.
                   07  RMS-PERF-F-VAL   PIC V99 VALUE 0.80.
                   07  RMS-PERF-C-VAL   PIC V99 VALUE 0.60.
                   07  RMS-PERF-FL-VAL  PIC V99 VALUE 1.20.
                   07  RMS-PERF-W-VAL   PIC V99 VALUE 0.90.
           03  RMS-DISCOUNT-FACTOR      PIC V99 VALUE 0.90.
           
      ******************************************************************
      * Transaction Processing Data Structures
      ******************************************************************
       01  TRANSACTION-DATA.
           03  TD-TRANSACTION-ID        PIC X(16).
           03  TD-TIMESTAMP             PIC X(26).
           03  TD-USER-ID               PIC X(8).
           03  TD-TERMINAL-ID           PIC X(8).
           03  TD-PROCESS-FLAGS.
               05  TD-COMMIT-FLAG       PIC X.
               05  TD-ROLLBACK-FLAG     PIC X.
               05  TD-RECOVERY-FLAG     PIC X.
               05  TD-SECURE-FLAG       PIC X.
           03  TD-PROCESS-DATA.
               05  TD-PROC-TYPE         PIC X(10).
               05  TD-PROC-SOURCE       PIC X(8).
               05  TD-PROC-TARGET       PIC X(8).
               05  TD-PROC-STATUS       PIC 9.
               05  TD-PROC-RESULT       PIC X(100). 