
       01  INPUT-RECORD.
           05 IN-RECORD-TYPE           PIC X(2).
              88 POLICY-APPLICATION    VALUE 'PA'.
              88 BUILDING-DETAIL       VALUE 'BD'.
              88 CLAIMS-HISTORY        VALUE 'CH'.
              88 FINANCIAL-DATA        VALUE 'FD'.
           
           05 IN-RECORD-VERSION        PIC X(2) VALUE '01'.
           05 IN-RECORD-LENGTH         PIC S9(4) COMP.
           
           05 IN-BASIC-DATA.
              10 IN-CUSTOMER-NUM       PIC X(10).
              10 IN-POLICY-TYPE        PIC X(1).
                 88 COMMERCIAL-POLICY  VALUE 'C'.
                 88 PERSONAL-POLICY    VALUE 'P'.
                 88 FARM-POLICY        VALUE 'F'.
              10 IN-POLICY-TERM        PIC 99.
              10 IN-EFFECTIVE-DATE     PIC 9(8).
              10 IN-APPLICATION-DATE   PIC 9(8).
           
           05 IN-PROPERTY-INFO.
              10 IN-PRIMARY-LOCATION.
                 15 IN-PROPERTY-TYPE   PIC X(15).
                 15 IN-POSTCODE        PIC X(8).
                 15 IN-ADDRESS         PIC X(60).
                 15 IN-LATITUDE        PIC S9(7)V9(6) COMP-3.
                 15 IN-LONGITUDE       PIC S9(8)V9(6) COMP-3.
                 15 IN-TERRITORY-CODE  PIC X(5).
                 15 IN-FLOOD-ZONE      PIC X(1).
                    88 HIGH-FLOOD-RISK VALUE 'A'.
                    88 MED-FLOOD-RISK  VALUE 'B' 'C'.
                    88 LOW-FLOOD-RISK  VALUE 'X'.
              
              10 IN-BUILDING-DETAILS.
                 15 IN-YEAR-BUILT      PIC 9(4).
                 15 IN-SQUARE-FOOTAGE  PIC 9(8).
                 15 IN-STORIES         PIC 99.
                 15 IN-CONSTRUCTION-TYPE PIC X(3).
                    88 FRAME-CONST     VALUE 'FRM'.
                    88 MASONRY-CONST   VALUE 'MAS'.
                    88 STEEL-CONST     VALUE 'STL'.
                    88 CONCRETE-CONST  VALUE 'CON'.
                 15 IN-OCCUPANCY-CODE  PIC X(5).
                 15 IN-SPRINKLER-IND   PIC X(1).
                    88 FULL-SPRINKLER  VALUE 'F'.
                    88 PARTIAL-SPRINK  VALUE 'P'.
                    88 NO-SPRINKLER    VALUE 'N'.
                 15 IN-ALARM-TYPE      PIC X(2).
                    88 CENTRAL-ALARM   VALUE 'CE'.
                    88 LOCAL-ALARM     VALUE 'LO'.
                    88 NO-ALARM        VALUE 'NO'.
           
           05 IN-CUSTOMER-INFO.
              10 IN-CUSTOMER-NAME      PIC X(50).
              10 IN-BUSINESS-TYPE      PIC X(20).
              10 IN-YEARS-IN-BUSINESS  PIC 99.
              10 IN-ANNUAL-REVENUE     PIC 9(10)V99.
              10 IN-EMPLOYEE-COUNT     PIC 9(5).
              10 IN-CUSTOMER-HISTORY   PIC X(1).
                 88 NEW-CUSTOMER       VALUE 'N'.
                 88 GOOD-CUSTOMER      VALUE 'G'.
                 88 RISKY-CUSTOMER     VALUE 'R'.
                 88 PREFERRED-CUSTOMER VALUE 'P'.
              10 IN-CREDIT-RATING      PIC X(3).
                 88 EXCELLENT-CREDIT   VALUE 'AAA' 'AA+' 'AA-'.
                 88 GOOD-CREDIT        VALUE 'A+' 'A' 'A-'.
                 88 FAIR-CREDIT        VALUE 'BBB' 'BB+' 'BB'.
                 88 POOR-CREDIT        VALUE 'B' 'C' 'D'.
           
           05 IN-COVERAGE-DATA.
              10 IN-COVERAGE-LIMITS.
                 15 IN-BUILDING-LIMIT  PIC 9(9)V99.
                 15 IN-CONTENTS-LIMIT  PIC 9(9)V99.
                 15 IN-BI-LIMIT        PIC 9(9)V99.
                 15 IN-LIABILITY-LIMIT PIC 9(9)V99.
              
              10 IN-DEDUCTIBLES.
                 15 IN-FIRE-DEDUCTIBLE PIC 9(6)V99.
                 15 IN-WIND-DEDUCTIBLE PIC 9(6)V99.
                 15 IN-FLOOD-DEDUCTIBLE PIC 9(6)V99.
                 15 IN-OTHER-DEDUCTIBLE PIC 9(6)V99.
              
              10 IN-PERIL-SELECTIONS.
                 15 IN-FIRE-PERIL      PIC 9(4).
                 15 IN-CRIME-PERIL     PIC 9(4).
                 15 IN-FLOOD-PERIL     PIC 9(4).
                 15 IN-WEATHER-PERIL   PIC 9(4).
                 15 IN-LIABILITY-PERIL PIC 9(4).
              
              10 IN-COVERAGE-MODIFIERS.
                 15 IN-REPLACEMENT-COST PIC X(1).
                    88 REPLACEMENT-YES VALUE 'Y'.
                 15 IN-AGREED-VALUE    PIC X(1).
                    88 AGREED-VALUE-YES VALUE 'Y'.
                 15 IN-COINSURANCE     PIC 999.
                 15 IN-INFLATION-GUARD PIC X(1).
                    88 INFLATION-YES   VALUE 'Y'.

           05 IN-FINANCIAL-INFO.
              10 IN-MORTGAGEE-INFO.
                 15 IN-MORTGAGEE-NAME  PIC X(40).
                 15 IN-LOAN-NUMBER     PIC X(20).
                 15 IN-LOAN-BALANCE    PIC 9(9)V99.
              10 IN-PRIOR-INSURANCE.
                 15 IN-PRIOR-CARRIER   PIC X(30).
                 15 IN-PRIOR-PREMIUM   PIC 9(7)V99.
                 15 IN-YEARS-WITH-PRIOR PIC 99.
                 15 IN-REASON-FOR-CHANGE PIC X(50).

           05 IN-CLAIMS-SUMMARY.
              10 IN-CLAIMS-COUNT-3YR   PIC 99.
              10 IN-CLAIMS-AMOUNT-3YR  PIC 9(8)V99.
              10 IN-LARGEST-CLAIM-AMT  PIC 9(8)V99.
              10 IN-LARGEST-CLAIM-DATE PIC 9(8).
              10 IN-CLAIMS-FREE-YEARS  PIC 99.

           05 FILLER                   PIC X(10).

      *----------------------------------------------------------------*
      * VARIABLE LENGTH DETAIL RECORDS                                *
      *----------------------------------------------------------------*
       01  VARIABLE-INPUT-RECORD.
           05 VIR-LENGTH               PIC S9(4) COMP.
           05 VIR-TYPE                 PIC X(2).
              88 BUILDING-DETAILS      VALUE 'BD'.
              88 CLAIMS-DETAIL         VALUE 'CD'.
              88 EQUIPMENT-DETAIL      VALUE 'ED'.
           05 VIR-CUSTOMER-NUM         PIC X(10).
           05 VIR-SEQUENCE             PIC 999.
           05 VIR-DATA                 PIC X(4000).
           
       01  VIR-BUILDING-DATA REDEFINES VIR-DATA.
           05 VIR-BUILDING-COUNT       PIC 99.
           05 VIR-BUILDING-ARRAY       OCCURS 1 TO 50 TIMES 
                                      DEPENDING ON VIR-BUILDING-COUNT
                                      INDEXED BY BLDG-IDX.
              10 VIR-BUILDING-ID       PIC X(5).
              10 VIR-BUILDING-TYPE     PIC X(20).
              10 VIR-BUILDING-USE      PIC X(30).
              10 VIR-SQ-FOOTAGE        PIC 9(8).
              10 VIR-CONSTRUCTION-YR   PIC 9(4).
              10 VIR-BUILDING-VALUE    PIC 9(9)V99.
              10 VIR-CONTENTS-VALUE    PIC 9(9)V99.
              10 VIR-SPECIAL-HAZARDS   PIC X(100).

       01  VIR-CLAIMS-DATA REDEFINES VIR-DATA.
           05 VIR-CLAIMS-COUNT         PIC 99.
           05 VIR-CLAIMS-ARRAY         OCCURS 1 TO 25 TIMES 
                                      DEPENDING ON VIR-CLAIMS-COUNT
                                      INDEXED BY CLAIMS-IDX.
              10 VIR-CLAIM-NUMBER      PIC X(15).
              10 VIR-CLAIM-DATE        PIC 9(8).
              10 VIR-LOSS-DATE         PIC 9(8).
              10 VIR-CLAIM-TYPE        PIC X(10).
              10 VIR-CAUSE-OF-LOSS     PIC X(20).
              10 VIR-CLAIM-STATUS      PIC X(10).
              10 VIR-PAID-AMOUNT       PIC 9(8)V99.
              10 VIR-RESERVE-AMOUNT    PIC 9(8)V99.
              10 VIR-CLAIM-NOTES       PIC X(100).

       01  VIR-EQUIPMENT-DATA REDEFINES VIR-DATA.
           05 VIR-EQUIPMENT-COUNT      PIC 99.
           05 VIR-EQUIPMENT-ARRAY      OCCURS 1 TO 30 TIMES 
                                      DEPENDING ON VIR-EQUIPMENT-COUNT
                                      INDEXED BY EQUIP-IDX.
              10 VIR-EQUIPMENT-ID      PIC X(10).
              10 VIR-EQUIPMENT-TYPE    PIC X(30).
              10 VIR-MANUFACTURER      PIC X(25).
              10 VIR-MODEL-NUMBER      PIC X(20).
              10 VIR-SERIAL-NUMBER     PIC X(25).
              10 VIR-PURCHASE-DATE     PIC 9(8).
              10 VIR-EQUIPMENT-VALUE   PIC 9(8)V99.
              10 VIR-REPLACEMENT-COST  PIC 9(8)V99.