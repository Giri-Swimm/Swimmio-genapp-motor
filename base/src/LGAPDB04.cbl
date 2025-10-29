       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGAPDB04.
      *================================================================*
      * PROGRAM: LGAPDB04 - ADVANCED ACTUARIAL PREMIUM CALCULATIONS   *
      *================================================================*
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       
       DATA DIVISION.
       
       WORKING-STORAGE SECTION.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *----------------------------------------------------------------*
      * ACTUARIAL CONSTANTS AND FACTORS                               *
      *----------------------------------------------------------------*
       01  WS-ACTUARIAL-CONSTANTS.
           05 WS-EXPENSE-RATIO         PIC V999 VALUE 0.350.
           05 WS-PROFIT-MARGIN         PIC V999 VALUE 0.150.
           05 WS-BASE-LOSS-RATIO       PIC V999 VALUE 0.600.
           05 WS-TREND-FACTOR          PIC V9999 VALUE 1.0350.
           05 WS-CREDIBILITY-FACTOR    PIC V999 VALUE 0.750.
           
       01  WS-PERIL-CORRELATIONS.
           05 WS-FIRE-WEATHER-CORR     PIC SV999 VALUE +0.250.
           05 WS-FLOOD-WEATHER-CORR    PIC SV999 VALUE +0.850.
           05 WS-CRIME-LOCATION-CORR   PIC SV999 VALUE +0.450.
           
       01  WS-CAT-MODEL-FACTORS.
           05 WS-HURRICANE-FACTOR      PIC V9999 VALUE 0.0125.
           05 WS-EARTHQUAKE-FACTOR     PIC V9999 VALUE 0.0080.
           05 WS-TORNADO-FACTOR        PIC V9999 VALUE 0.0045.
           05 WS-FLOOD-FACTOR          PIC V9999 VALUE 0.0090.

      *----------------------------------------------------------------*
      * RATE TABLE STRUCTURES                                         *
      *----------------------------------------------------------------*
       01  WS-BASE-RATE-TABLE.
           05 WS-RATE-LOOKUP           OCCURS 4 TIMES
                                       INDEXED BY RATE-IDX.
              10 WS-PERIL-CODE         PIC X(2).
              10 WS-TERRITORY-RATES    OCCURS 10 TIMES.
                 15 WS-CONSTRUCTION-RATES OCCURS 5 TIMES.
                    20 WS-OCCUPANCY-RATES OCCURS 8 TIMES.
                       25 WS-BASE-RATE  PIC V9(6).
                       25 WS-MIN-PREM   PIC 9(5)V99.
                       25 WS-MAX-PREM   PIC 9(7)V99.

      *----------------------------------------------------------------*
      * CALCULATION WORK AREAS                                        *
      *----------------------------------------------------------------*
       01  WS-CALCULATION-AREAS.
           05 WS-INTERMEDIATE-RESULTS.
              10 WS-BASE-PREMIUM       PIC 9(9)V99.
              10 WS-MODIFIED-PREMIUM   PIC 9(9)V99.
              10 WS-CAT-LOADING        PIC 9(8)V99.
              10 WS-EXPENSE-LOADING    PIC 9(8)V99.
              10 WS-PROFIT-LOADING     PIC 9(8)V99.
              10 WS-TAX-AMOUNT         PIC 9(7)V99.
              10 WS-TOTAL-BEFORE-DISC  PIC 9(10)V99.
              
           05 WS-DISCOUNT-CALCULATIONS.
              10 WS-MULTI-PERIL-DISC   PIC V999.
              10 WS-CLAIMS-FREE-DISC   PIC V999.
              10 WS-DEDUCTIBLE-CREDIT  PIC V999.
              10 WS-SCHEDULE-MOD       PIC SV999.
              10 WS-EXPERIENCE-MOD     PIC V9999.
              10 WS-TOTAL-DISCOUNT     PIC V999.
              
           05 WS-EXPOSURE-CALCULATIONS.
              10 WS-BUILDING-EXPOSURE  PIC 9(10)V99.
              10 WS-CONTENTS-EXPOSURE  PIC 9(10)V99.
              10 WS-BI-EXPOSURE        PIC 9(10)V99.
              10 WS-TOTAL-INSURED-VAL  PIC 9(11)V99.
              10 WS-EXPOSURE-DENSITY   PIC V9999.

      *----------------------------------------------------------------*
      * LINKAGE SECTION FOR PARAMETER PASSING                        *
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  LK-INPUT-DATA.
           05 LK-CUSTOMER-NUM          PIC X(10).
           05 LK-RISK-SCORE            PIC 999.
           05 LK-PROPERTY-TYPE         PIC X(15).
           05 LK-TERRITORY             PIC X(5).
           05 LK-CONSTRUCTION-TYPE     PIC X(3).
           05 LK-OCCUPANCY-CODE        PIC X(5).
           05 LK-PROTECTION-CLASS      PIC X(2).
           05 LK-YEAR-BUILT            PIC 9(4).
           05 LK-SQUARE-FOOTAGE        PIC 9(8).
           05 LK-YEARS-IN-BUSINESS     PIC 99.
           05 LK-CLAIMS-COUNT-5YR      PIC 99.
           05 LK-CLAIMS-AMOUNT-5YR     PIC 9(9)V99.
           
       01  LK-COVERAGE-DATA.
           05 LK-COVERAGE-LIMITS.
              10 LK-BUILDING-LIMIT     PIC 9(9)V99.
              10 LK-CONTENTS-LIMIT     PIC 9(9)V99.
              10 LK-BI-LIMIT           PIC 9(9)V99.
           05 LK-DEDUCTIBLES.
              10 LK-FIRE-DEDUCTIBLE    PIC 9(6)V99.
              10 LK-WIND-DEDUCTIBLE    PIC 9(6)V99.
              10 LK-FLOOD-DEDUCTIBLE   PIC 9(6)V99.
              10 LK-OTHER-DEDUCTIBLE   PIC 9(6)V99.
           05 LK-PERIL-SELECTIONS.
              10 LK-FIRE-PERIL         PIC 9(4).
              10 LK-CRIME-PERIL        PIC 9(4).
              10 LK-FLOOD-PERIL        PIC 9(4).
              10 LK-WEATHER-PERIL      PIC 9(4).
              
       01  LK-OUTPUT-RESULTS.
           05 LK-CALCULATED-PREMIUMS.
              10 LK-FIRE-PREMIUM       PIC 9(8)V99.
              10 LK-CRIME-PREMIUM      PIC 9(8)V99.
              10 LK-FLOOD-PREMIUM      PIC 9(8)V99.
              10 LK-WEATHER-PREMIUM    PIC 9(8)V99.
              10 LK-TOTAL-PREMIUM      PIC 9(9)V99.
           05 LK-PREMIUM-COMPONENTS.
              10 LK-BASE-AMOUNT        PIC 9(9)V99.
              10 LK-CAT-LOAD-AMT       PIC 9(7)V99.
              10 LK-EXPENSE-LOAD-AMT   PIC 9(7)V99.
              10 LK-PROFIT-LOAD-AMT    PIC 9(7)V99.
              10 LK-DISCOUNT-AMT       PIC 9(7)V99.
              10 LK-TAX-AMT            PIC 9(6)V99.
           05 LK-RATING-FACTORS.
              10 LK-EXPERIENCE-MOD     PIC V9999.
              10 LK-SCHEDULE-MOD       PIC SV999.
              10 LK-FINAL-RATE-FACTOR  PIC V9999.
              
       PROCEDURE DIVISION USING LK-INPUT-DATA, LK-COVERAGE-DATA, 
                               LK-OUTPUT-RESULTS.
       
       P100-MAIN.
           PERFORM P200-INIT
           PERFORM P300-RATES
           PERFORM P350-EXPOSURE
           PERFORM P400-EXP-MOD
           PERFORM P500-SCHED-MOD
           PERFORM P600-BASE-PREM
           PERFORM P700-CAT-LOAD
           PERFORM P800-EXPENSE
           PERFORM P900-DISC
           PERFORM P950-TAXES
           PERFORM P999-FINAL
           GOBACK.

       P200-INIT.
           INITIALIZE WS-CALCULATION-AREAS
           INITIALIZE WS-BASE-RATE-TABLE
           
           COMPUTE WS-BUILDING-EXPOSURE = 
               LK-BUILDING-LIMIT * (1 + (LK-RISK-SCORE - 100) / 1000)
               
           COMPUTE WS-CONTENTS-EXPOSURE = 
               LK-CONTENTS-LIMIT * (1 + (LK-RISK-SCORE - 100) / 1000)
               
           COMPUTE WS-BI-EXPOSURE = 
               LK-BI-LIMIT * (1 + (LK-RISK-SCORE - 100) / 1000)
               
           COMPUTE WS-TOTAL-INSURED-VAL = 
               WS-BUILDING-EXPOSURE + WS-CONTENTS-EXPOSURE + 
               WS-BI-EXPOSURE
               
           IF LK-SQUARE-FOOTAGE > ZERO
               COMPUTE WS-EXPOSURE-DENSITY = 
                   WS-TOTAL-INSURED-VAL / LK-SQUARE-FOOTAGE
           ELSE
               MOVE 100.00 TO WS-EXPOSURE-DENSITY
           END-IF.

      *----------------------------------------------------------------*
      * LOAD BASE RATES FROM DATABASE TABLES                         *
      *----------------------------------------------------------------*
       LOAD-RATE-TABLES.
           EXEC SQL
               SELECT BASE_RATE, MIN_PREMIUM, MAX_PREMIUM
               INTO :WS-BASE-RATE, :WS-MIN-PREM, :WS-MAX-PREM
               FROM RATE_MASTER
               WHERE TERRITORY = :LK-TERRITORY
                 AND CONSTRUCTION_TYPE = :LK-CONSTRUCTION-TYPE
                 AND OCCUPANCY_CODE = :LK-OCCUPANCY-CODE
                 AND PERIL_CODE = 'FI'
                 AND EFFECTIVE_DATE <= CURRENT DATE
                 AND EXPIRY_DATE >= CURRENT DATE
           END-EXEC
           
           IF SQLCODE = 0
               MOVE WS-BASE-RATE TO 
                    WS-BASE-RATE (1, 1, 1, 1)
           ELSE
               MOVE 0.008500 TO WS-BASE-RATE (1, 1, 1, 1)
           END-IF
           
      *    Load rates for other perils (CRIME, FLOOD, WEATHER)
           PERFORM P310-PERIL-RATES VARYING RATE-IDX FROM 2 BY 1 
                   UNTIL RATE-IDX > 4.

       P310-PERIL-RATES.
           EVALUATE RATE-IDX
               WHEN 2  MOVE 'CR' TO WS-PERIL-CODE (RATE-IDX)
               WHEN 3  MOVE 'FL' TO WS-PERIL-CODE (RATE-IDX)  
               WHEN 4  MOVE 'WE' TO WS-PERIL-CODE (RATE-IDX)
           END-EVALUATE
           
           EXEC SQL
               SELECT BASE_RATE, MIN_PREMIUM, MAX_PREMIUM
               INTO :WS-BASE-RATE, :WS-MIN-PREM, :WS-MAX-PREM
               FROM RATE_MASTER
               WHERE TERRITORY = :LK-TERRITORY
                 AND PERIL_CODE = :WS-PERIL-CODE (RATE-IDX)
                 AND EFFECTIVE_DATE <= CURRENT DATE
                 AND EXPIRY_DATE >= CURRENT DATE
           END-EXEC
           
           IF SQLCODE = 0
               MOVE WS-BASE-RATE TO 
                    WS-BASE-RATE (RATE-IDX, 1, 1, 1)
           ELSE
               EVALUATE RATE-IDX
                   WHEN 2  MOVE 0.006200 TO 
                          WS-BASE-RATE (RATE-IDX, 1, 1, 1)
                   WHEN 3  MOVE 0.012800 TO 
                          WS-BASE-RATE (RATE-IDX, 1, 1, 1)
                   WHEN 4  MOVE 0.009600 TO 
                          WS-BASE-RATE (RATE-IDX, 1, 1, 1)
               END-EVALUATE
           END-IF.

       P400-EXP-MOD.
           MOVE 1.0000 TO WS-EXPERIENCE-MOD
           
           IF LK-YEARS-IN-BUSINESS >= 5
               IF LK-CLAIMS-COUNT-5YR = ZERO
                   MOVE 0.8500 TO WS-EXPERIENCE-MOD
               ELSE
                   COMPUTE WS-EXPERIENCE-MOD = 
                       1.0000 + 
                       ((LK-CLAIMS-AMOUNT-5YR / WS-TOTAL-INSURED-VAL) * 
                        WS-CREDIBILITY-FACTOR * 0.50)
                   
                   IF WS-EXPERIENCE-MOD > 2.0000
                       MOVE 2.0000 TO WS-EXPERIENCE-MOD
                   END-IF
                   
                   IF WS-EXPERIENCE-MOD < 0.5000
                       MOVE 0.5000 TO WS-EXPERIENCE-MOD
                   END-IF
               END-IF
           ELSE
               MOVE 1.1000 TO WS-EXPERIENCE-MOD
           END-IF
           
           MOVE WS-EXPERIENCE-MOD TO LK-EXPERIENCE-MOD.

       P500-SCHED-MOD.
           MOVE +0.000 TO WS-SCHEDULE-MOD
           
      *    Building age factor
           EVALUATE TRUE
               WHEN LK-YEAR-BUILT >= 2010
                   SUBTRACT 0.050 FROM WS-SCHEDULE-MOD
               WHEN LK-YEAR-BUILT >= 1990
                   CONTINUE
               WHEN LK-YEAR-BUILT >= 1970
                   ADD 0.100 TO WS-SCHEDULE-MOD
               WHEN OTHER
                   ADD 0.200 TO WS-SCHEDULE-MOD
           END-EVALUATE
           
      *    Protection class factor
           EVALUATE LK-PROTECTION-CLASS
               WHEN '01' THRU '03'
                   SUBTRACT 0.100 FROM WS-SCHEDULE-MOD
               WHEN '04' THRU '06'
                   SUBTRACT 0.050 FROM WS-SCHEDULE-MOD
               WHEN '07' THRU '09'
                   CONTINUE
               WHEN OTHER
                   ADD 0.150 TO WS-SCHEDULE-MOD
           END-EVALUATE
           
      *    Occupancy hazard factor
           EVALUATE LK-OCCUPANCY-CODE
               WHEN 'OFF01' THRU 'OFF05'
                   SUBTRACT 0.025 FROM WS-SCHEDULE-MOD
               WHEN 'MFG01' THRU 'MFG10'
                   ADD 0.075 TO WS-SCHEDULE-MOD
               WHEN 'WHS01' THRU 'WHS05'
                   ADD 0.125 TO WS-SCHEDULE-MOD
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           
      *    Exposure density factor
           IF WS-EXPOSURE-DENSITY > 500.00
               ADD 0.100 TO WS-SCHEDULE-MOD
           ELSE
               IF WS-EXPOSURE-DENSITY < 50.00
                   SUBTRACT 0.050 FROM WS-SCHEDULE-MOD
               END-IF
           END-IF
           
           IF WS-SCHEDULE-MOD > +0.400
               MOVE +0.400 TO WS-SCHEDULE-MOD
           END-IF
           
           IF WS-SCHEDULE-MOD < -0.200
               MOVE -0.200 TO WS-SCHEDULE-MOD
           END-IF
           
           MOVE WS-SCHEDULE-MOD TO LK-SCHEDULE-MOD.

       P600-BASE-PREM.
           MOVE ZERO TO LK-BASE-AMOUNT
           
      * FIRE PREMIUM
           IF LK-FIRE-PERIL > ZERO
               COMPUTE LK-FIRE-PREMIUM = 
                   (WS-BUILDING-EXPOSURE + WS-CONTENTS-EXPOSURE) *
                   WS-BASE-RATE (1, 1, 1, 1) * 
                   WS-EXPERIENCE-MOD *
                   (1 + WS-SCHEDULE-MOD) *
                   WS-TREND-FACTOR
                   
               ADD LK-FIRE-PREMIUM TO LK-BASE-AMOUNT
           END-IF
           
      * CRIME PREMIUM
           IF LK-CRIME-PERIL > ZERO
               COMPUTE LK-CRIME-PREMIUM = 
                   (WS-CONTENTS-EXPOSURE * 0.80) *
                   WS-BASE-RATE (2, 1, 1, 1) * 
                   WS-EXPERIENCE-MOD *
                   (1 + WS-SCHEDULE-MOD) *
                   WS-TREND-FACTOR
                   
               ADD LK-CRIME-PREMIUM TO LK-BASE-AMOUNT
           END-IF
           
      * FLOOD PREMIUM
           IF LK-FLOOD-PERIL > ZERO
               COMPUTE LK-FLOOD-PREMIUM = 
                   WS-BUILDING-EXPOSURE *
                   WS-BASE-RATE (3, 1, 1, 1) * 
                   WS-EXPERIENCE-MOD *
                   (1 + WS-SCHEDULE-MOD) *
                   WS-TREND-FACTOR * 1.25
                   
               ADD LK-FLOOD-PREMIUM TO LK-BASE-AMOUNT
           END-IF
           
      * WEATHER PREMIUM
           IF LK-WEATHER-PERIL > ZERO
               COMPUTE LK-WEATHER-PREMIUM = 
                   (WS-BUILDING-EXPOSURE + WS-CONTENTS-EXPOSURE) *
                   WS-BASE-RATE (4, 1, 1, 1) * 
                   WS-EXPERIENCE-MOD *
                   (1 + WS-SCHEDULE-MOD) *
                   WS-TREND-FACTOR
                   
               ADD LK-WEATHER-PREMIUM TO LK-BASE-AMOUNT
           END-IF.

       P700-CAT-LOAD.
           MOVE ZERO TO WS-CAT-LOADING
           
      * Hurricane loading (wind/weather peril)
           IF LK-WEATHER-PERIL > ZERO
               COMPUTE WS-CAT-LOADING = WS-CAT-LOADING +
                   (LK-WEATHER-PREMIUM * WS-HURRICANE-FACTOR)
           END-IF
           
      * Earthquake loading (affects all perils)  
           COMPUTE WS-CAT-LOADING = WS-CAT-LOADING +
               (LK-BASE-AMOUNT * WS-EARTHQUAKE-FACTOR)
           
      * Tornado loading (weather peril primarily)
           IF LK-WEATHER-PERIL > ZERO
               COMPUTE WS-CAT-LOADING = WS-CAT-LOADING +
                   (LK-WEATHER-PREMIUM * WS-TORNADO-FACTOR)
           END-IF
           
      * Flood cat loading (if flood coverage selected)
           IF LK-FLOOD-PERIL > ZERO
               COMPUTE WS-CAT-LOADING = WS-CAT-LOADING +
                   (LK-FLOOD-PREMIUM * WS-FLOOD-FACTOR)
           END-IF
           
           MOVE WS-CAT-LOADING TO LK-CAT-LOAD-AMT.

       P800-EXPENSE.
           COMPUTE WS-EXPENSE-LOADING = 
               (LK-BASE-AMOUNT + LK-CAT-LOAD-AMT) * WS-EXPENSE-RATIO
               
           COMPUTE WS-PROFIT-LOADING = 
               (LK-BASE-AMOUNT + LK-CAT-LOAD-AMT + WS-EXPENSE-LOADING) *
               WS-PROFIT-MARGIN
               
           MOVE WS-EXPENSE-LOADING TO LK-EXPENSE-LOAD-AMT
           MOVE WS-PROFIT-LOADING TO LK-PROFIT-LOAD-AMT.

       P900-DISC.
           MOVE ZERO TO WS-TOTAL-DISCOUNT
           
      * Multi-peril discount
           MOVE ZERO TO WS-MULTI-PERIL-DISC
           IF LK-FIRE-PERIL > ZERO AND
              LK-CRIME-PERIL > ZERO AND
              LK-FLOOD-PERIL > ZERO AND
              LK-WEATHER-PERIL > ZERO
               MOVE 0.100 TO WS-MULTI-PERIL-DISC
           ELSE
               IF LK-FIRE-PERIL > ZERO AND
                  LK-WEATHER-PERIL > ZERO AND
                  (LK-CRIME-PERIL > ZERO OR LK-FLOOD-PERIL > ZERO)
                   MOVE 0.050 TO WS-MULTI-PERIL-DISC
               END-IF
           END-IF
           
      * Claims-free discount  
           MOVE ZERO TO WS-CLAIMS-FREE-DISC
           IF LK-CLAIMS-COUNT-5YR = ZERO AND LK-YEARS-IN-BUSINESS >= 5
               MOVE 0.075 TO WS-CLAIMS-FREE-DISC
           END-IF
           
      * Deductible credit
           MOVE ZERO TO WS-DEDUCTIBLE-CREDIT
           IF LK-FIRE-DEDUCTIBLE >= 10000
               ADD 0.025 TO WS-DEDUCTIBLE-CREDIT
           END-IF
           IF LK-WIND-DEDUCTIBLE >= 25000  
               ADD 0.035 TO WS-DEDUCTIBLE-CREDIT
           END-IF
           IF LK-FLOOD-DEDUCTIBLE >= 50000
               ADD 0.045 TO WS-DEDUCTIBLE-CREDIT
           END-IF
           
           COMPUTE WS-TOTAL-DISCOUNT = 
               WS-MULTI-PERIL-DISC + WS-CLAIMS-FREE-DISC + 
               WS-DEDUCTIBLE-CREDIT
               
           IF WS-TOTAL-DISCOUNT > 0.250
               MOVE 0.250 TO WS-TOTAL-DISCOUNT
           END-IF
           
           COMPUTE LK-DISCOUNT-AMT = 
               (LK-BASE-AMOUNT + LK-CAT-LOAD-AMT + 
                LK-EXPENSE-LOAD-AMT + LK-PROFIT-LOAD-AMT) *
               WS-TOTAL-DISCOUNT.

       P950-TAXES.
           COMPUTE WS-TAX-AMOUNT = 
               (LK-BASE-AMOUNT + LK-CAT-LOAD-AMT + 
                LK-EXPENSE-LOAD-AMT + LK-PROFIT-LOAD-AMT - 
                LK-DISCOUNT-AMT) * 0.0675
                
           MOVE WS-TAX-AMOUNT TO LK-TAX-AMT.

       P999-FINAL.
           COMPUTE LK-TOTAL-PREMIUM = 
               LK-BASE-AMOUNT + LK-CAT-LOAD-AMT + 
               LK-EXPENSE-LOAD-AMT + LK-PROFIT-LOAD-AMT -
               LK-DISCOUNT-AMT + LK-TAX-AMT
               
           COMPUTE LK-FINAL-RATE-FACTOR = 
               LK-TOTAL-PREMIUM / WS-TOTAL-INSURED-VAL
               
           IF LK-FINAL-RATE-FACTOR > 0.050000
               MOVE 0.050000 TO LK-FINAL-RATE-FACTOR
               COMPUTE LK-TOTAL-PREMIUM = 
                   WS-TOTAL-INSURED-VAL * LK-FINAL-RATE-FACTOR
           END-IF.