       01  OUTPUT-RECORD.
           05 OUT-CUSTOMER-NUM         PIC X(10).
           05 FILLER                   PIC X VALUE SPACE.
           05 OUT-PROPERTY-TYPE        PIC X(15).
           05 FILLER                   PIC X VALUE SPACE.
           05 OUT-POSTCODE             PIC X(8).
           05 FILLER                   PIC X VALUE SPACE.
           05 OUT-RISK-SCORE           PIC ZZ9.
           05 FILLER                   PIC X VALUE SPACE.
           05 OUT-FIRE-PREMIUM         PIC ZZZ,ZZ9.99.
           05 FILLER                   PIC X VALUE SPACE.
           05 OUT-CRIME-PREMIUM        PIC ZZZ,ZZ9.99.
           05 FILLER                   PIC X VALUE SPACE.
           05 OUT-FLOOD-PREMIUM        PIC ZZZ,ZZ9.99.
           05 FILLER                   PIC X VALUE SPACE.
           05 OUT-WEATHER-PREMIUM      PIC ZZZ,ZZ9.99.
           05 FILLER                   PIC X VALUE SPACE.
           05 OUT-TOTAL-PREMIUM        PIC Z,ZZZ,ZZ9.99.
           05 FILLER                   PIC X VALUE SPACE.
           05 OUT-STATUS               PIC X(20).
           05 FILLER                   PIC X VALUE SPACE.
           05 OUT-REJECT-REASON        PIC X(50). 