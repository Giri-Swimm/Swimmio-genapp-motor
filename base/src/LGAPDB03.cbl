       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGAPDB03.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       
       DATA DIVISION.
       
       WORKING-STORAGE SECTION.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           
       01  WS-FIRE-FACTOR              PIC V99 VALUE 0.80.
       01  WS-CRIME-FACTOR             PIC V99 VALUE 0.60.
       01  WS-FLOOD-FACTOR             PIC V99 VALUE 1.20.
       01  WS-WEATHER-FACTOR           PIC V99 VALUE 0.90.
       
       LINKAGE SECTION.
       01  LK-RISK-SCORE               PIC 999.
       01  LK-FIRE-PERIL               PIC 9(4).
       01  LK-CRIME-PERIL              PIC 9(4).
       01  LK-FLOOD-PERIL              PIC 9(4).
       01  LK-WEATHER-PERIL            PIC 9(4).
       01  LK-STAT                     PIC 9.
       01  LK-STAT-DESC                PIC X(20).
       01  LK-REJ-RSN                  PIC X(50).
       01  LK-FIRE-PREMIUM             PIC 9(8)V99.
       01  LK-CRIME-PREMIUM            PIC 9(8)V99.
       01  LK-FLOOD-PREMIUM            PIC 9(8)V99.
       01  LK-WEATHER-PREMIUM          PIC 9(8)V99.
       01  LK-TOTAL-PREMIUM            PIC 9(9)V99.
       01  LK-DISC-FACT                PIC V99.
       
       PROCEDURE DIVISION USING LK-RISK-SCORE, LK-FIRE-PERIL, LK-CRIME-PERIL,
                                LK-FLOOD-PERIL, LK-WEATHER-PERIL, LK-STAT,
                                LK-STAT-DESC, LK-REJ-RSN, LK-FIRE-PREMIUM,
                                LK-CRIME-PREMIUM, LK-FLOOD-PREMIUM,
                                LK-WEATHER-PREMIUM, LK-TOTAL-PREMIUM,
                                LK-DISC-FACT.
       
       MAIN-LOGIC.
           PERFORM GET-RISK-FACTORS
           PERFORM CALCULATE-VERDICT
           PERFORM CALCULATE-PREMIUMS
           GOBACK.
       
       GET-RISK-FACTORS.
           EXEC SQL
               SELECT FACTOR_VALUE INTO :WS-FIRE-FACTOR
               FROM RISK_FACTORS
               WHERE PERIL_TYPE = 'FIRE'
           END-EXEC.
           
           IF SQLCODE = 0
               CONTINUE
           ELSE
               MOVE 0.80 TO WS-FIRE-FACTOR
           END-IF.
           
           EXEC SQL
               SELECT FACTOR_VALUE INTO :WS-CRIME-FACTOR
               FROM RISK_FACTORS
               WHERE PERIL_TYPE = 'CRIME'
           END-EXEC.
           
           IF SQLCODE = 0
               CONTINUE
           ELSE
               MOVE 0.60 TO WS-CRIME-FACTOR
           END-IF.
       
       CALCULATE-VERDICT.
           IF LK-RISK-SCORE > 200
             MOVE 2 TO LK-STAT
             MOVE 'REJECTED' TO LK-STAT-DESC
             MOVE 'High Risk Score - Manual Review Required' 
               TO LK-REJ-RSN
           ELSE
             IF LK-RISK-SCORE > 150
               MOVE 1 TO LK-STAT
               MOVE 'PENDING' TO LK-STAT-DESC
               MOVE 'Medium Risk - Pending Review'
                 TO LK-REJ-RSN
             ELSE
               MOVE 0 TO LK-STAT
               MOVE 'APPROVED' TO LK-STAT-DESC
               MOVE SPACES TO LK-REJ-RSN
             END-IF
           END-IF.
       
       CALCULATE-PREMIUMS.
           MOVE 1.00 TO LK-DISC-FACT
           
           IF LK-FIRE-PERIL > 0 AND
              LK-CRIME-PERIL > 0 AND
              LK-FLOOD-PERIL > 0 AND
              LK-WEATHER-PERIL > 0
             MOVE 0.90 TO LK-DISC-FACT
           END-IF

           COMPUTE LK-FIRE-PREMIUM =
             ((LK-RISK-SCORE * WS-FIRE-FACTOR) * LK-FIRE-PERIL *
               LK-DISC-FACT)
           
           COMPUTE LK-CRIME-PREMIUM =
             ((LK-RISK-SCORE * WS-CRIME-FACTOR) * LK-CRIME-PERIL *
               LK-DISC-FACT)
           
           COMPUTE LK-FLOOD-PREMIUM =
             ((LK-RISK-SCORE * WS-FLOOD-FACTOR) * LK-FLOOD-PERIL *
               LK-DISC-FACT)
           
           COMPUTE LK-WEATHER-PREMIUM =
             ((LK-RISK-SCORE * WS-WEATHER-FACTOR) * LK-WEATHER-PERIL *
               LK-DISC-FACT)

           COMPUTE LK-TOTAL-PREMIUM = 
             LK-FIRE-PREMIUM + LK-CRIME-PREMIUM + 
             LK-FLOOD-PREMIUM + LK-WEATHER-PREMIUM. 