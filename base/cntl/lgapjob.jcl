//LGAPJOB  JOB (ACCT123),'POLICY PREMIUM CALC',
//         CLASS=A,
//         MSGCLASS=H,
//         MSGLEVEL=(1,1),
//         NOTIFY=&SYSUID,
//         REGION=4M,
//         TIME=30
//*
//*================================================================*
//* JOB: LGAPJOB - INSURANCE POLICY PREMIUM CALCULATION BATCH     *
//* DESC: PROCESSES POLICY APPLICATIONS AND CALCULATES PREMIUMS   *
//* FREQ: DAILY AT 02:00 AM                                       *
//* DEPS: RISK_FACTORS TABLE, RATE_TABLES, CONFIG FILES           *
//*================================================================*
//*
//JOBLIB   DD DSN=LGAP.PROD.LOADLIB,DISP=SHR
//         DD DSN=DB2.V12.SDSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*
//*----------------------------------------------------------------*
//* STEP 01: DATA PREPARATION AND VALIDATION                      *
//*----------------------------------------------------------------*
//STEP01   EXEC PGM=SORT,REGION=2M
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=LGAP.INPUT.RAW.DATA,DISP=SHR
//SORTOUT  DD DSN=LGAP.INPUT.SORTED,DISP=(NEW,PASS),
//         UNIT=SYSDA,SPACE=(CYL,(5,2)),
//         DCB=(RECFM=FB,LRECL=300,BLKSIZE=27000)
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A,11,1,CH,A)
  OUTREC FIELDS=(1,300)
/*
//*
//*----------------------------------------------------------------*
//* STEP 02: DATABASE PREPARATION                                  *
//*----------------------------------------------------------------*
//STEP02   EXEC PGM=IKJEFT01,REGION=2M,
//         DYNAMNBR=20,COND=(0,NE)
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN PROGRAM(DSNTEP2) PLAN(LGAPPLAN) -
      LIB('LGAP.PROD.DBRMLIB')
  END
//SYSIN    DD *
  -- REFRESH RISK FACTOR CACHE
  DELETE FROM LGAP.RISK_CACHE WHERE LOAD_DATE < CURRENT DATE - 1 DAYS;
  
  -- UPDATE RATE TABLE EFFECTIVE DATES
  UPDATE LGAP.RATE_MASTER 
  SET STATUS = 'ACTIVE' 
  WHERE EFFECTIVE_DATE <= CURRENT DATE 
    AND EXPIRY_DATE >= CURRENT DATE;
    
  COMMIT;
/*
//*
//*----------------------------------------------------------------*
//* STEP 03: MAIN PREMIUM CALCULATION PROCESSING                   *
//*----------------------------------------------------------------*
//STEP03   EXEC PGM=LGAPDB01,REGION=4M,
//         COND=((0,NE,STEP01),(4,LT,STEP02))
//STEPLIB  DD DSN=LGAP.PROD.LOADLIB,DISP=SHR
//         DD DSN=DB2.V12.SDSNLOAD,DISP=SHR
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*,OUTLIM=500
//SYSABEND DD SYSOUT=*
//*
//* INPUT FILES
//INPUT    DD DSN=LGAP.INPUT.SORTED,DISP=(OLD,DELETE)
//CONFIG   DD DSN=LGAP.CONFIG.MASTER,DISP=SHR
//RATES    DD DSN=LGAP.RATE.TABLES,DISP=SHR
//*
//* OUTPUT FILES  
//OUTPUT   DD DSN=LGAP.OUTPUT.PREMIUM.DATA,
//         DISP=(NEW,CATLG,DELETE),
//         UNIT=SYSDA,SPACE=(CYL,(10,5)),
//         DCB=(RECFM=FB,LRECL=200,BLKSIZE=27800)
//REJECTED DD DSN=LGAP.OUTPUT.REJECTED.DATA,
//         DISP=(NEW,CATLG,DELETE),
//         UNIT=SYSDA,SPACE=(CYL,(2,1)),
//         DCB=(RECFM=FB,LRECL=200,BLKSIZE=27800)
//SUMMARY  DD DSN=LGAP.OUTPUT.SUMMARY.RPT,
//         DISP=(NEW,CATLG,DELETE),
//         UNIT=SYSDA,SPACE=(CYL,(1,1)),
//         DCB=(RECFM=FB,LRECL=132,BLKSIZE=27984)
//*
//* DATABASE CONNECTIONS
//DSNPLAN  DD DSN=LGAP.PROD.PLAN.LGAPPLAN,DISP=SHR
//*
//* TEMPORARY WORK FILES
//SORTWK01 DD UNIT=SYSDA,SPACE=(CYL,(5,2))
//SORTWK02 DD UNIT=SYSDA,SPACE=(CYL,(5,2))
//SORTWK03 DD UNIT=SYSDA,SPACE=(CYL,(5,2))
//*
//*----------------------------------------------------------------*
//* STEP 04: GENERATE PROCESSING REPORTS                          *
//*----------------------------------------------------------------*
//STEP04   EXEC PGM=LGAPRPT1,REGION=2M,
//         COND=((0,NE,STEP01),(8,LT,STEP02),(8,LT,STEP03))
//STEPLIB  DD DSN=LGAP.PROD.LOADLIB,DISP=SHR
//SYSOUT   DD SYSOUT=*
//INPUT    DD DSN=LGAP.OUTPUT.PREMIUM.DATA,DISP=SHR
//REPORT   DD DSN=LGAP.REPORTS.DAILY.SUMMARY,
//         DISP=(NEW,CATLG,DELETE),
//         UNIT=SYSDA,SPACE=(TRK,(50,10)),
//         DCB=(RECFM=FBA,LRECL=133,BLKSIZE=27930)
//*
//*----------------------------------------------------------------*
//* STEP 05: BACKUP AND CLEANUP                                   *
//*----------------------------------------------------------------*
//STEP05   EXEC PGM=IEBGENER,REGION=1M,
//         COND=((0,NE,STEP01),(8,LT,STEP02),(8,LT,STEP03))
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=LGAP.OUTPUT.PREMIUM.DATA,DISP=SHR
//SYSUT2   DD DSN=LGAP.BACKUP.PREMIUM.G0001V00,
//         DISP=(NEW,CATLG,DELETE),
//         UNIT=TAPE,LABEL=(1,SL)
//*
//NOTIFY   EXEC PGM=IEBGENER,REGION=1M,
//         COND=((0,NE,STEP01),(4,LT,STEP03))
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY  
//SYSUT1   DD *
JOB LGAPJOB COMPLETED SUCCESSFULLY
PROCESSING SUMMARY AVAILABLE IN LGAP.OUTPUT.SUMMARY.RPT
BACKUP CREATED: LGAP.BACKUP.PREMIUM.G0001V00
/*
//SYSUT2   DD SYSOUT=(,INTRDR)
//*
//*================================================================*