---
title: Motor Policy Menu Screen (SSMAPP1)
---
The Motor Policy Menu screen (SSMAPP1) provides users with a central interface to inquire, add, delete, or update motor insurance policies. It collects and displays all relevant motor policy details and guides users through the available policy management actions.

## Screen Preview

```
SSP1        General Insurance Motor Policy Menu

    1. Policy Inquiry 
    2. Policy Add     
    3. Policy Delete  
    4. Policy Update  

         Policy Number      ____________
         Cust Number        ____________
         Issue date         ____________ (yyyy-mm-dd)
         Expiry date        ____________ (yyyy-mm-dd)
         Car Make           ____________________
         Car Model          ____________________
         Car Value          ______
         Registration       _________
         Car Colour         ________
         CC                 ________
         Manufacture Date   __________ (yyyy-mm-dd)
         No. of Accidents   ______
         Policy Premium     ______

    Select Option   _


[Error/Status Message Area]

ENTER=Continue  PF3=Exit  CLEAR=Clear
```

## Fields

### Policy Number (ENP1PNO)

- Length: 10 characters
- Right-justified, zero-filled
- Input field for the motor policy number
- No explicit validation in the provided code, but typically must be a valid policy number

### Cust Number (ENP1CNO)

- Length: 10 characters
- Right-justified, zero-filled
- Input field for the customer number
- No explicit validation in the provided code, but typically must be a valid customer number

### Issue Date (ENP1IDA)

- Length: 10 characters
- Format: yyyy-mm-dd
- Input field for the policy issue date
- No explicit validation in the provided code

### Expiry Date (ENP1EDA)

- Length: 10 characters
- Format: yyyy-mm-dd
- Input field for the policy expiry date
- No explicit validation in the provided code

### Car Make (ENP1CMK)

- Length: 20 characters
- Input field for the car make
- No explicit validation in the provided code

### Car Model (ENP1CMO)

- Length: 20 characters
- Input field for the car model
- No explicit validation in the provided code

### Car Value (ENP1VAL)

- Length: 6 characters
- Right-justified, zero-filled
- Input field for the car's value
- No explicit validation in the provided code

### Registration (ENP1REG)

- Length: 7 characters
- Input field for the car registration number
- No explicit validation in the provided code

### Car Colour (ENP1COL)

- Length: 8 characters
- Input field for the car colour
- No explicit validation in the provided code

### CC (ENP1CC)

- Length: 8 characters
- Right-justified, zero-filled
- Input field for the car's engine capacity (CC)
- No explicit validation in the provided code

### Manufacture Date (ENP1MAN)

- Length: 10 characters
- Format: yyyy-mm-dd
- Input field for the car's manufacture date
- No explicit validation in the provided code

### No. of Accidents (ENP1ACC)

- Length: 6 characters
- Right-justified, zero-filled
- Input field for the number of accidents
- No explicit validation in the provided code

### Policy Premium (ENP1PRE)

- Length: 6 characters
- Right-justified, zero-filled
- Input field for the policy premium
- No explicit validation in the provided code

### Select Option (ENP1OPT)

- Length: 1 character
- Numeric input only
- Must be entered (MUSTENTER)
- Used to select menu option (1-4)
- Validation: If not valid, error message is shown

### Error/Status Message Area (ERP1FLD)

- Length: 40 characters
- Output only
- Used to display error or status messages
- Populated by the program logic (e.g., 'Motor Policy Updated', 'Error Adding Motor Policy')

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
