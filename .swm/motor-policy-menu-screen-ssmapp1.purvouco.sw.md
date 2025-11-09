---
title: Motor Policy Menu Screen (SSMAPP1)
---
The Motor Policy Menu screen (SSMAPP1) provides users with a central interface to manage motor insurance policies. Users can inquire, add, delete, or update motor policies by entering relevant details and selecting the desired option.

## Screen Preview

```
SSP1        General Insurance Motor Policy Menu

    1. Policy Inquiry 
    2. Policy Add     
    3. Policy Delete  
    4. Policy Update  

         Policy Number      ____________
         Cust Number        ____________
         Issue date         ____________   (yyyy-mm-dd)
         Expiry date        ____________   (yyyy-mm-dd)
         Car Make           ____________________
         Car Model          ____________________
         Car Value          ______
         Registration       _________
         Car Colour         ________
         CC                 ________
         Manufacture Date   ____________   (yyyy-mm-dd)
         No. of Accidents   ______
         Policy Premium     ______

    Select Option   _


[Error/Status Message Area]

ENTER=Continue  PF3=End  CLEAR=Clear
```

## Fields

### Policy Number (ENP1PNO)

- Length: 10 characters
- Input field, right-justified, zero-filled
- Used for identifying the motor policy
- Required for inquiry, add, delete, and update operations
- No explicit validation in BMS, but program expects a valid number

### Customer Number (ENP1CNO)

- Length: 10 characters
- Input field, right-justified, zero-filled
- Used for identifying the customer
- Required for all operations
- No explicit validation in BMS, but program expects a valid number

### Issue Date (ENP1IDA)

- Length: 10 characters
- Input field
- Format: yyyy-mm-dd
- Used for add/update operations
- No explicit validation in BMS, but program expects valid date

### Expiry Date (ENP1EDA)

- Length: 10 characters
- Input field
- Format: yyyy-mm-dd
- Used for add/update operations
- No explicit validation in BMS, but program expects valid date

### Car Make (ENP1CMK)

- Length: 20 characters
- Input field
- Used for add/update operations
- No explicit validation in BMS or program

### Car Model (ENP1CMO)

- Length: 20 characters
- Input field
- Used for add/update operations
- No explicit validation in BMS or program

### Car Value (ENP1VAL)

- Length: 6 characters
- Input field, right-justified, zero-filled
- Used for add/update operations
- No explicit validation in BMS or program

### Registration (ENP1REG)

- Length: 7 characters
- Input field
- Used for add/update operations
- No explicit validation in BMS or program

### Car Colour (ENP1COL)

- Length: 8 characters
- Input field
- Used for add/update operations
- No explicit validation in BMS or program

### CC (ENP1CC)

- Length: 8 characters
- Input field, right-justified, zero-filled
- Used for add/update operations
- No explicit validation in BMS or program

### Manufacture Date (ENP1MAN)

- Length: 10 characters
- Input field
- Format: yyyy-mm-dd
- Used for add/update operations
- No explicit validation in BMS or program

### No. of Accidents (ENP1ACC)

- Length: 6 characters
- Input field, right-justified, zero-filled
- Used for add/update operations
- No explicit validation in BMS or program

### Policy Premium (ENP1PRE)

- Length: 6 characters
- Input field, right-justified, zero-filled
- Used for add/update operations
- No explicit validation in BMS or program

### Select Option (ENP1OPT)

- Length: 1 character
- Numeric input only
- Must be entered (MUSTENTER validation)
- Used to select menu option (1-4)
- Program expects 1, 2, 3, or 4

### Error/Status Message Area (ERP1FLD)

- Length: 40 characters
- Output only, protected
- Used to display error or status messages
- Populated by program logic (e.g., 'Motor Policy Updated', 'Error Adding Motor Policy')

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
