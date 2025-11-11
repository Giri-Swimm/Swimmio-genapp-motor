---
title: General Insurance Motor Policy Menu
---
The Motor Policy Menu screen allows users to inquire, add, delete, or update motor insurance policies by entering or viewing policy and vehicle details. It serves as the main entry point for all motor policy transactions in the system.

## Screen Preview

```
SSP1        General Insurance Motor Policy Menu  

        1. Policy Inquiry 
        2. Policy Add     
        3. Policy Delete  
        4. Policy Update  

              Policy Number   ____________
              Cust Number     ____________
              Issue date      ____________ (yyyy-mm-dd)
              Expiry date     ____________ (yyyy-mm-dd)
              Car Make        ____________________
              Car Model       ____________________
              Car Value       ______
              Registration    ________
              Car Colour      ________
              CC              ________
              Manufacture Date __________ (yyyy-mm-dd)
              No. of Accidents ______
              Policy Premium  ______

        Select Option _


        [                                        ]

ENTER=Continue  F3=Back  F4=Clear
```

## Fields

### Policy Number (ENP1PNO)

- Length: 10 characters
- Input field, right-justified, zero-filled
- Used for identifying the policy
- No explicit validation in BMS, but must be numeric in COBOL context

### Customer Number (ENP1CNO)

- Length: 10 characters
- Input field, right-justified, zero-filled
- Used for identifying the customer
- No explicit validation in BMS, but must be numeric in COBOL context

### Issue Date (ENP1IDA)

- Length: 10 characters
- Input field
- Expected format: yyyy-mm-dd
- No explicit validation in BMS, but date format expected in COBOL

### Expiry Date (ENP1EDA)

- Length: 10 characters
- Input field
- Expected format: yyyy-mm-dd
- No explicit validation in BMS, but date format expected in COBOL

### Car Make (ENP1CMK)

- Length: 20 characters
- Input field
- No explicit validation in BMS or COBOL

### Car Model (ENP1CMO)

- Length: 20 characters
- Input field
- No explicit validation in BMS or COBOL

### Car Value (ENP1VAL)

- Length: 6 characters
- Input field, right-justified, zero-filled
- Numeric value expected (COBOL: PIC 9(6))

### Registration (ENP1REG)

- Length: 7 characters
- Input field
- No explicit validation in BMS or COBOL

### Car Colour (ENP1COL)

- Length: 8 characters
- Input field
- No explicit validation in BMS or COBOL

### CC (ENP1CC)

- Length: 8 characters
- Input field, right-justified, zero-filled
- Numeric value expected (COBOL: PIC 9(4)), but BMS allows 8 chars

### Manufacture Date (ENP1MAN)

- Length: 10 characters
- Input field
- Expected format: yyyy-mm-dd
- No explicit validation in BMS, but date format expected in COBOL

### No. of Accidents (ENP1ACC)

- Length: 6 characters
- Input field, right-justified, zero-filled
- Numeric value expected (COBOL: PIC 9(6))

### Policy Premium (ENP1PRE)

- Length: 6 characters
- Input field, right-justified, zero-filled
- Numeric value expected (COBOL: PIC 9(6))

### Select Option (ENP1OPT)

- Length: 1 character
- Input field, numeric only
- Must be entered (VALIDN=MUSTENTER)
- Used to select menu option (1-4)
- Error message shown if invalid

### Error/Status Message (ERP1FLD)

- Length: 40 characters
- Output only (protected)
- Used to display error or status messages from COBOL logic
- Not user-editable

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
