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