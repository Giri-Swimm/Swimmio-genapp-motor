       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGAPVS01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  V1-RCD1                   PIC S9(8) COMP.
       01  V1-RCD2                   PIC S9(8) COMP.
       01  V1-LEN                    PIC S9(8) COMP.
       01  V1-CODE                   PIC XX Value spaces.
       01  V1-SYS                    PIC X(4) Value spaces.
       01  V1-COMM                   PIC S9(4) COMP.
      ******************************
       01  V2-RECORD.
         03  V2-KEY.
           05  V2-REQ                  Pic X.
           05  V2-CUST                 Pic X(10).
           05  V2-POL                  Pic X(10).
         03 V2-DATA                    Pic X(83).
         03 V2-C-DATA Redefines V2-DATA.
           05  V2-C-PCD                Pic X(8).
           05  V2-C-Z9                 Pic 9(4).
           05  V2-C-CUST               Pic X(31).
           05  V2-C-VAL                Pic 999.
           05  V2-C-P1VAL              Pic 9(8).
           05  V2-C-P2VAL              Pic 9(8).
           05  V2-C-P3VAL              Pic 9(8).
           05  V2-C-P4VAL              Pic 9(8).
         03 V2-E-DATA Redefines V2-DATA.
           05  V2-E-OPT1               Pic X.
           05  V2-E-OPT2               Pic X.
           05  V2-E-OPT3               Pic X.
           05  V2-E-NAME               Pic X(10).
           05  V2-E-LIFE               Pic X(30).
         03 V2-H-DATA Redefines V2-DATA.
           05  V2-H-TYPE               Pic X(15).
           05  V2-H-ROOMS              Pic 999.
           05  V2-H-COST               Pic 9(8).
           05  V2-H-PCD                Pic X(8).
           05  V2-H-NAME               Pic X(9).
         03 V2-M-DATA Redefines V2-DATA.
           05  V2-M-MAKE               Pic X(15).
           05  V2-M-MODEL              Pic X(15).
           05  V2-M-COST               Pic 9(6).
           05  V2-M-NUM                Pic X(7).
      ******************************
      * Variables for time/date processing
       01  V3-TIME                     PIC S9(8) COMP VALUE +0.
       01  V3-DATE1                    PIC X(8)  VALUE SPACES.
       01  V3-DATE2                    PIC X(10) VALUE SPACES.
      * Error Message structure
       01  ERROR-MSG.
           03 EM-DATE                  PIC X(8)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-TIME                  PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X(9)  VALUE ' LGAPVS01'.
           03 EM-VARIABLE.
             05 FILLER                 PIC X(6)  VALUE ' PNUM='.
             05 EM-POLNUM              PIC X(10)  VALUE SPACES.
             05 FILLER                 PIC X(6)  VALUE ' CNUM='.
             05 EM-CUSNUM              PIC X(10)  VALUE SPACES.
             05 FILLER                 PIC X(20)
                                        Value ' Write file KSDSPOLY'.
             05 FILLER                 PIC X(6)  VALUE ' RESP='.
             05 EM-RESPRC              PIC +9(5) USAGE DISPLAY.
             05 FILLER                 PIC X(7)  VALUE ' RESP2='.
             05 EM-RESP2RC             PIC +9(5) USAGE DISPLAY.

       01  CA-ERROR-MSG.
           03 FILLER                   PIC X(9)  VALUE 'COMMAREA='.
           03 CA-DATA                  PIC X(90) VALUE SPACES.

      ******************************
       77 Eyecatcher               PIC X(16)
                                      Value 'Program LGAPVS01'.
      *****************************************************************
      *    L I N K A G E     S E C T I O N
      *****************************************************************
       LINKAGE SECTION.
       01  DFHCOMMAREA.
         Copy LGCMAREA.

      *----------------------------------------------------------------*
      *****************************************************************
       PROCEDURE DIVISION.

      *---------------------------------------------------------------*
       P100-ENTRY SECTION.
      *
      *---------------------------------------------------------------*
           Move EIBCALEN To V1-COMM.
      *---------------------------------------------------------------*
           Move CA-Request-ID(4:1) To V2-REQ
           Move CA-Policy-Num      To V2-POL
           Move CA-Customer-Num    To V2-CUST

           Evaluate V2-REQ

             When 'C'
               Move CA-B-PST     To V2-C-PCD
               Move CA-B-ST       To V2-C-Z9
               Move CA-B-Customer     To V2-C-CUST
               Move WS-RISK-SCORE     To V2-C-VAL
               Move CA-B-CA-B-FPR  To V2-C-P1VAL
               Move CA-B-CPR To V2-C-P2VAL
               Move CA-B-FLPR To V2-C-P3VAL
               Move CA-B-WPR To V2-C-P4VAL

             When 'E'
               Move CA-E-W-PRO        To  V2-E-OPT1
               Move CA-E-EQU          To  V2-E-OPT2
               Move CA-E-M-FUN        To  V2-E-OPT3
               Move CA-E-FUND-NAME    To  V2-E-NAME
               Move CA-E-LIFE-ASSURED To  V2-E-LIFE

             When 'H'
               Move CA-H-P-TYP         To  V2-H-TYPE
               Move CA-H-BED           To  V2-H-ROOMS
               Move CA-H-VAL           To  V2-H-COST
               Move CA-H-PCD           To  V2-H-PCD
               Move CA-H-H-NAM         To  V2-H-NAME

             When 'M'
               Move CA-M-MAKE          To  V2-M-MAKE
               Move CA-M-MODEL         To  V2-M-MODEL
               Move CA-M-VALUE         To  V2-M-COST
               Move CA-M-REGNUMBER     To  V2-M-NUM

             When Other
               Move Spaces To V2-DATA
           End-Evaluate

      *---------------------------------------------------------------*
           Exec CICS Write File('KSDSPOLY')
                     From(V2-RECORD)
                     Length(104)
                     Ridfld(V2-KEY)
                     KeyLength(21)
                     RESP(V1-RCD1)
           End-Exec.
           If V1-RCD1 Not = DFHRESP(NORMAL)
             Move EIBRESP2 To V1-RCD2
             MOVE '80' TO CA-RETURN-CODE
             PERFORM P999-ERROR
             EXEC CICS RETURN END-EXEC
           End-If.

      *---------------------------------------------------------------*

       P100-EXIT.
           EXIT.
           GOBACK.
      *---------------------------------------------------------------*
       P999-ERROR.
           EXEC CICS ASKTIME ABSTIME(V3-TIME)
           END-EXEC
           EXEC CICS FORMATTIME ABSTIME(V3-TIME)
                     MMDDYYYY(V3-DATE1)
                     TIME(V3-DATE2)
           END-EXEC
      *
           MOVE V3-DATE1 TO EM-DATE
           MOVE V3-DATE2 TO EM-TIME
           Move CA-Customer-Num To EM-Cusnum
           Move CA-Policy-Num   To EM-POLNUM 
           Move V1-RCD1         To EM-RespRC
           Move V1-RCD2         To EM-Resp2RC
           EXEC CICS LINK PROGRAM('LGSTSQ')
                     COMMAREA(ERROR-MSG)
                     LENGTH(LENGTH OF ERROR-MSG)
           END-EXEC.
           IF EIBCALEN > 0 THEN
             IF EIBCALEN < 91 THEN
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CA-DATA
               EXEC CICS LINK PROGRAM('LGSTSQ')
                         COMMAREA(CA-ERROR-MSG)
                         LENGTH(Length Of CA-ERROR-MSG)
               END-EXEC
             ELSE
               MOVE DFHCOMMAREA(1:90) TO CA-DATA
               EXEC CICS LINK PROGRAM('LGSTSQ')
                         COMMAREA(CA-ERROR-MSG)
                         LENGTH(Length Of CA-ERROR-MSG)
               END-EXEC
             END-IF
           END-IF.
           EXIT.
