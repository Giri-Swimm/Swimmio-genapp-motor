       01  WS-IN-STAT                  PIC X(2).
           88 INPUT-OK                 VALUE '00'.
           88 INPUT-EOF                VALUE '10'.
           
       01  WS-OUT-STAT                 PIC X(2).
           88 OUTPUT-OK                VALUE '00'.

       01  WS-CONFIG-STAT              PIC X(2).
           88 CONFIG-OK                VALUE '00'.
           88 CONFIG-EOF               VALUE '10'.

       01  WS-RATE-STAT                PIC X(2).
           88 RATE-OK                  VALUE '00'.
           88 RATE-EOF                 VALUE '10'.
       01  WS-PROCESSING-COUNTERS.
           05 WS-REC-CNT               PIC 9(7) VALUE ZERO.
           05 WS-ERR-CNT               PIC 9(6) VALUE ZERO.
           05 WS-PROC-CNT              PIC 9(7) VALUE ZERO.
           05 WS-REJECTED-CNT          PIC 9(6) VALUE ZERO.
           05 WS-WARNING-CNT           PIC 9(6) VALUE ZERO.

       01  WS-RISK-ANALYSIS.
           05 WS-BASE-RISK-SCR         PIC 999 VALUE ZERO.
           05 WS-ADJUSTED-RISK-SCR     PIC 999 VALUE ZERO.
           05 WS-FINAL-RISK-SCR        PIC 999 VALUE ZERO.
           
           05 WS-PROPERTY-RISKS.
              10 WS-STRUCTURAL-RISK    OCCURS 5 TIMES
                                       INDEXED BY STRUCT-IDX.
                 15 WS-RISK-TYPE       PIC X(10).
                 15 WS-RISK-WEIGHT     PIC V999.
                 15 WS-RISK-SCORE      PIC 999.
                 15 WS-RISK-CATEGORY   PIC X(1).
                    88 FIRE-RISK       VALUE 'F'.
                    88 STRUCTURAL-RISK VALUE 'S'.
                    88 LOCATION-RISK   VALUE 'L'.
                    88 OCCUPANCY-RISK  VALUE 'O'.
                    88 PROTECTION-RISK VALUE 'P'.
           
           05 WS-GEOGRAPHICAL-RISKS.
              10 WS-ZONE-DATA          OCCURS 3 TIMES
                                       INDEXED BY ZONE-IDX.
                 15 WS-ZONE-TYPE       PIC X(15).
                 15 WS-ZONE-MULTIPLIER PIC V99.
                 15 WS-CAT-EXPOSURE    PIC 9(3).
                 15 WS-TERRITORY-CODE  PIC X(5).
           
           05 WS-CUSTOMER-PROFILE.
              10 WS-EXPERIENCE-MOD     PIC V999.
              10 WS-CREDIT-SCORE       PIC 999.
              10 WS-YEARS-IN-BUSINESS  PIC 99.
              10 WS-CLAIM-COUNT        PIC 99.
              10 WS-CLAIM-HISTORY      OCCURS 10 TIMES
                                       INDEXED BY CLAIM-IDX.
                 15 WS-CLAIM-DATE      PIC 9(8).
                 15 WS-CLAIM-AMOUNT    PIC 9(8)V99.
                 15 WS-CLAIM-TYPE      PIC X(10).
                 15 WS-CLAIM-STATUS    PIC X(1).
                    88 OPEN-CLAIM      VALUE 'O'.
                    88 CLOSED-CLAIM    VALUE 'C'.
                    88 DENIED-CLAIM    VALUE 'D'.

       01  WS-ACTUARIAL-DATA.
           05 WS-EXPOSURE-VALUES.
              10 WS-TIV                PIC 9(10)V99.
              10 WS-BUILDING-VALUE     PIC 9(9)V99.
              10 WS-CONTENTS-VALUE     PIC 9(9)V99.
              10 WS-BI-VALUE           PIC 9(9)V99.
           
           05 WS-RATE-FACTORS.
              10 WS-BASE-RATES         OCCURS 4 TIMES.
                 15 WS-PERIL-RATE      PIC V9(6).
                 15 WS-MIN-PREMIUM     PIC 9(6)V99.
                 15 WS-MAX-PREMIUM     PIC 9(7)V99.
              10 WS-TERRITORIAL-FACTOR PIC V999.
              10 WS-CONSTRUCTION-FACTOR PIC V999.
              10 WS-OCCUPANCY-FACTOR   PIC V999.
              10 WS-PROTECTION-FACTOR  PIC V999.
           
           05 WS-CAT-MODELING.
              10 WS-HURRICANE-AAL      PIC 9(8)V99.
              10 WS-EARTHQUAKE-AAL     PIC 9(8)V99.
              10 WS-FLOOD-AAL          PIC 9(8)V99.
              10 WS-TORNADO-AAL        PIC 9(8)V99.
              10 WS-CAT-LOADING        PIC V9999.

       01  WS-PREMIUM-BREAKDOWN.
           05 WS-BASE-PREMIUMS.
              10 WS-FR-BASE-PREM       PIC 9(8)V99.
              10 WS-CR-BASE-PREM       PIC 9(8)V99.
              10 WS-FL-BASE-PREM       PIC 9(8)V99.
              10 WS-WE-BASE-PREM       PIC 9(8)V99.
           
           05 WS-LOADINGS-DISCOUNTS.
              10 WS-CAT-LOAD-AMT       PIC 9(7)V99.
              10 WS-EXPENSE-LOAD       PIC 9(7)V99.
              10 WS-PROFIT-LOAD        PIC 9(7)V99.
              10 WS-MULTI-PERIL-DISC   PIC 9(6)V99.
              10 WS-CLAIMS-FREE-DISC   PIC 9(6)V99.
           
           05 WS-FINAL-PREMIUMS.
              10 WS-FR-PREM            PIC 9(8)V99.
              10 WS-CR-PREM            PIC 9(8)V99.
              10 WS-FL-PREM            PIC 9(8)V99.
              10 WS-WE-PREM            PIC 9(8)V99.
              10 WS-TOT-PREM           PIC 9(9)V99.
           
           05 WS-TAXES-FEES.
              10 WS-STATE-TAX          PIC 9(6)V99.
              10 WS-COUNTY-TAX         PIC 9(5)V99.
              10 WS-POLICY-FEE         PIC 9(4)V99.
              10 WS-INSPECTION-FEE     PIC 9(4)V99.

       01  WS-DECISION-DATA.
           05 WS-UNDERWRITING-DECISION.
              10 WS-STAT               PIC 9 VALUE 0.
                 88 UW-APPROVED        VALUE 0.
                 88 UW-PENDING         VALUE 1.
                 88 UW-REJECTED        VALUE 2.
                 88 UW-REFERRED        VALUE 3.
              10 WS-STAT-DESC          PIC X(20).
              10 WS-REJ-RSN            PIC X(50).
              10 WS-UW-NOTES           PIC X(100).
           
           05 WS-DISCOUNT-ELIGIBILITY.
              10 WS-MULTI-POLICY-ELIG  PIC X VALUE 'N'.
                 88 MULTI-POLICY-YES   VALUE 'Y'.
              10 WS-CLAIMS-FREE-ELIG   PIC X VALUE 'N'.
                 88 CLAIMS-FREE-YES    VALUE 'Y'.
              10 WS-SAFETY-PROG-ELIG   PIC X VALUE 'N'.
                 88 SAFETY-PROG-YES    VALUE 'Y'.
           
           05 WS-DISC-FACT             PIC V99 VALUE 1.00.
           05 WS-TOTAL-DISC-FACT       PIC V99 VALUE 1.00.

       01  WS-RATE-TABLE-DATA.
           05 WS-RATE-EFFECTIVE-DATE   PIC 9(8).
           05 WS-RATE-TERRITORY        PIC X(5).
           05 WS-RATE-CONSTRUCTION     PIC X(3).
           05 WS-RATE-OCCUPANCY        PIC X(5).
           05 WS-RATE-PROTECTION       PIC X(2).

       01  WS-ERROR-HANDLING.
           05 WS-ERROR-ARRAY           OCCURS 20 TIMES
                                       INDEXED BY ERR-IDX.
              10 WS-ERROR-CODE         PIC X(6).
              10 WS-ERROR-SEVERITY     PIC X(1).
                 88 FATAL-ERROR        VALUE 'F'.
                 88 WARNING-ERROR      VALUE 'W'.
                 88 INFO-ERROR         VALUE 'I'.
              10 WS-ERROR-FIELD        PIC X(20).
              10 WS-ERROR-MESSAGE      PIC X(80).
           
           05 WS-ERROR-COUNT           PIC 99.
           05 WS-WARNING-COUNT         PIC 99.
           05 WS-ERR-MSG               PIC X(100).

       01  WS-CALCULATION-WORK.
           05 WS-TEMP-AMOUNT           PIC 9(10)V99.
           05 WS-TEMP-RATE             PIC V9(8).
           05 WS-TEMP-FACTOR           PIC V9999.
           05 WS-TEMP-PERCENTAGE       PIC 999V99.
           05 WS-WORK-DATE             PIC 9(8).
           05 WS-WORK-COUNTER          PIC 9(4).

       01  WS-CONTROL-TOTALS.
           05 WS-INPUT-HASH-TOTAL      PIC 9(12)V99.
           05 WS-OUTPUT-HASH-TOTAL     PIC 9(12)V99.
           05 WS-PREMIUM-TOTAL         PIC 9(12)V99.
           05 WS-POLICY-COUNT          PIC 9(8).
           05 WS-PROCESS-START-TIME    PIC X(8).
           05 WS-PROCESS-END-TIME      PIC X(8).