01  INPUT-RECORD.
           05 IN-CUSTOMER-NUM          PIC X(10).
           05 IN-POLICY-TYPE           PIC X(1).
           05 IN-PROPERTY-TYPE         PIC X(15).
           05 IN-POSTCODE              PIC X(8).
           05 IN-ADDRESS               PIC X(60).
           05 IN-LATITUDE              PIC S9(7)V9(6) COMP-3.
           05 IN-LONGITUDE             PIC S9(8)V9(6) COMP-3.
           05 IN-CUSTOMER-NAME         PIC X(50).
           05 IN-FIRE-PERIL            PIC 9(4).
           05 IN-CRIME-PERIL           PIC 9(4).
           05 IN-FLOOD-PERIL           PIC 9(4).
           05 IN-WEATHER-PERIL         PIC 9(4).
           05 IN-FIRE-COVERAGE         PIC 9(8)V99.
           05 IN-CRIME-COVERAGE        PIC 9(8)V99.
           05 IN-FLOOD-COVERAGE        PIC 9(8)V99.
           05 IN-WEATHER-COVERAGE      PIC 9(8)V99.
           05 IN-CUSTOMER-HISTORY      PIC X(1).
               88 NEW-CUSTOMER         VALUE 'N'.
               88 GOOD-CUSTOMER        VALUE 'G'.
               88 RISKY-CUSTOMER       VALUE 'R'.
           05 FILLER                   PIC X(5).