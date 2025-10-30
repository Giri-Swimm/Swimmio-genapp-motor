---
title: Endowment Policy Menu (LGTESTP2) - Overview
---
# Overview

This document explains the flow of handling endowment policy transactions through a menu-driven interface. Users can inquire about, add, delete, or update policies, with each operation routed to backend logic for validation, data retrieval, premium calculation, and record management. The flow ensures business rules are enforced and provides user feedback for all operations.

```mermaid
flowchart TD
  node1["Startup and Initial Request Check"]:::HeadingStyle --> node2["User Operation Dispatch"]:::HeadingStyle
  click node1 goToHeading "Startup and Initial Request Check"
  click node2 goToHeading "User Operation Dispatch"
  node2 --> node3["Policy Inquiry Request Handling"]:::HeadingStyle
  click node3 goToHeading "Policy Inquiry Request Handling"
  node2 --> node4["Policy Add Validation and Error Logging"]:::HeadingStyle
  click node4 goToHeading "Policy Add Validation and Error Logging"
  node2 --> node5["Validating and Executing Policy Deletion Requests"]:::HeadingStyle
  click node5 goToHeading "Validating and Executing Policy Deletion Requests"
  node2 --> node6["Validating and Routing Policy Update Requests"]:::HeadingStyle
  click node6 goToHeading "Validating and Routing Policy Update Requests"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Technical Overview

```mermaid
sequenceDiagram
  participant MENU as LGTESTP2.cbl<br/>*(Menu-driven transaction handler)*
  participant INQUIRY as LGIPOL01.cbl<br/>*(Policy inquiry processor)*
  participant RETRIEVE as LGIPDB01.cbl<br/>*(Policy data retriever)*
  participant QUEUE as LGSTS2Q.cbl<br/>*(Error and message queue handler)*
  participant ADD as LGAPOL01.cbl<br/>*(Policy add request validator)*
  participant PREMIUM as base/src/LGAPDB01.cbl<br/>*(Premium calculator and record processor)*
  participant RISK as base/src/LGAPDB03.cbl<br/>*(Risk factor and premium computation)*
  participant ACTUARY as base/src/LGAPDB04.cbl<br/>*(Advanced actuarial calculator)*
  participant DELETE as LGDPOL01.cbl<br/>*(Policy deletion validator)*
  participant DELDB as LGDPDB01.cbl<br/>*(Policy record deleter)*
  participant DELVSAM as LGDPVS01.cbl<br/>*(VSAM policy record deleter)*
  participant UPDATE as LGUPOL01.cbl<br/>*(Policy update validator)*
  participant UPDB as LGUPDB01.cbl<br/>*(Policy record updater)*
  participant UPVSAM as LGUPVS01.cbl<br/>*(VSAM policy record updater)*

  MENU->>INQUIRY: Dispatch inquiry request
  INQUIRY->>RETRIEVE: Retrieve policy data
  INQUIRY->>QUEUE: Queue error/status messages

  MENU->>ADD: Dispatch add request
  ADD->>PREMIUM: Process premium calculation and record insertion
  PREMIUM->>RISK: Compute risk factors and premiums
  PREMIUM->>ACTUARY: Run advanced actuarial calculations

  MENU->>DELETE: Dispatch delete request
  DELETE->>DELDB: Delete policy record from DB2
  DELDB->>DELVSAM: Delete policy record from VSAM

  MENU->>UPDATE: Dispatch update request
  UPDATE->>UPDB: Update policy record in DB2
  UPDB->>UPVSAM: Update policy record in VSAM

%% Swimm:
%% sequenceDiagram
%%   participant MENU as LGTESTP2.cbl<br/>*(Menu-driven transaction handler)*
%%   participant INQUIRY as LGIPOL01.cbl<br/>*(Policy inquiry processor)*
%%   participant RETRIEVE as LGIPDB01.cbl<br/>*(Policy data retriever)*
%%   participant QUEUE as LGSTS2Q.cbl<br/>*(Error and message queue handler)*
%%   participant ADD as LGAPOL01.cbl<br/>*(Policy add request validator)*
%%   participant PREMIUM as <SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath><br/>*(Premium calculator and record processor)*
%%   participant RISK as <SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath><br/>*(Risk factor and premium computation)*
%%   participant ACTUARY as <SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath><br/>*(Advanced actuarial calculator)*
%%   participant DELETE as LGDPOL01.cbl<br/>*(Policy deletion validator)*
%%   participant DELDB as LGDPDB01.cbl<br/>*(Policy record deleter)*
%%   participant DELVSAM as LGDPVS01.cbl<br/>*(VSAM policy record deleter)*
%%   participant UPDATE as LGUPOL01.cbl<br/>*(Policy update validator)*
%%   participant UPDB as LGUPDB01.cbl<br/>*(Policy record updater)*
%%   participant UPVSAM as LGUPVS01.cbl<br/>*(VSAM policy record updater)*
%% 
%%   MENU->>INQUIRY: Dispatch inquiry request
%%   INQUIRY->>RETRIEVE: Retrieve policy data
%%   INQUIRY->>QUEUE: Queue error/status messages
%% 
%%   MENU->>ADD: Dispatch add request
%%   ADD->>PREMIUM: Process premium calculation and record insertion
%%   PREMIUM->>RISK: Compute risk factors and premiums
%%   PREMIUM->>ACTUARY: Run advanced actuarial calculations
%% 
%%   MENU->>DELETE: Dispatch delete request
%%   DELETE->>DELDB: Delete policy record from DB2
%%   DELDB->>DELVSAM: Delete policy record from VSAM
%% 
%%   MENU->>UPDATE: Dispatch update request
%%   UPDATE->>UPDB: Update policy record in DB2
%%   UPDB->>UPVSAM: Update policy record in VSAM
```

## Dependencies

### Programs

- LGIPOL01 (<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>)
- LGIPDB01 (<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)
- LGAPOL01 (<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>)
- LGAPDB01 (<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>)
- LGAPDB02
- LGAPDB03 (<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>)
- LGAPDB04 (<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>)
- LGDPOL01 (<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>)
- LGDPDB01 (<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>)
- LGDPVS01 (<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>)
- LGUPOL01 (<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>)
- LGUPDB01 (<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>)
- LGUPVS01 (<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>)

### Copybooks

- SQLCA
- LGPOLICY (<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>)
- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)
- INPUTREC2 (<SwmPath>[base/src/INPUTREC2.cpy](base/src/INPUTREC2.cpy)</SwmPath>)
- OUTPUTREC (<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>)
- WORKSTOR (<SwmPath>[base/src/WORKSTOR.cpy](base/src/WORKSTOR.cpy)</SwmPath>)
- LGAPACT (<SwmPath>[base/src/LGAPACT.cpy](base/src/LGAPACT.cpy)</SwmPath>)
- SSMAP

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
