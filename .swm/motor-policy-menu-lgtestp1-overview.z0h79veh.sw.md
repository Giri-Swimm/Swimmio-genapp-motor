---
title: Motor Policy Menu (LGTESTP1) - Overview
---
# Overview

This document describes the flow for managing motor insurance policies through a menu-driven interface. Users can view, add, delete, or update policies, with each operation validated and processed by backend systems, and results displayed to the user.

```mermaid
flowchart TD
  node1["Menu Input Handling
(Menu Input Handling)"]:::HeadingStyle --> node2{"Which operation?
(Menu Input Handling)"}:::HeadingStyle
  click node1 goToHeading "Menu Input Handling"
  click node2 goToHeading "Menu Input Handling"
  node2 -->|"View"| node3["Policy Inquiry Dispatch"]:::HeadingStyle
  click node3 goToHeading "Policy Inquiry Dispatch"
  node3 --> node4{"Is data available?"}
  node4 -->|"Yes"| node5["Menu Display and Session End"]:::HeadingStyle
  click node5 goToHeading "Menu Display and Session End"
  node4 -->|"No"| node6["No Data Message and Exit"]:::HeadingStyle
  click node6 goToHeading "No Data Message and Exit"
  node6 --> node5
  node2 -->|"Add"| node7["Policy Data Validation and Insert"]:::HeadingStyle
  click node7 goToHeading "Policy Data Validation and Insert"
  node7 --> node5
  node2 -->|"Delete"| node8["Delete Request Validation and Routing"]:::HeadingStyle
  click node8 goToHeading "Delete Request Validation and Routing"
  node8 --> node5
  node2 -->|"Update"| node9["Policy Update Validation and Routing"]:::HeadingStyle
  click node9 goToHeading "Policy Update Validation and Routing"
  node9 --> node5

classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Technical Overview

```mermaid
sequenceDiagram
  participant MENU as LGTESTP1.cbl<br/>*(Motor Policy Menu Controller)*
  participant INQUIRY as LGIPOL01.cbl<br/>*(Policy Inquiry Dispatcher)*
  participant INQDB as LGIPDB01.cbl<br/>*(Policy Data Fetcher)*
  participant ADD as LGAPOL01.cbl<br/>*(Policy Addition Validator)*
  participant ADDDB as base/src/LGAPDB01.cbl<br/>*(Policy Addition Processor)*
  participant DEL as LGDPOL01.cbl<br/>*(Policy Deletion Validator)*
  participant DELDB as LGDPDB01.cbl<br/>*(Policy Deletion Processor)*
  participant DELVSAM as LGDPVS01.cbl<br/>*(VSAM Policy File Deletion Handler)*
  participant UPD as LGUPOL01.cbl<br/>*(Policy Update Validator)*
  participant UPDDB as LGUPDB01.cbl<br/>*(Policy Update Processor)*
  participant UPDVSAM as LGUPVS01.cbl<br/>*(VSAM Policy File Update Handler)*
  participant LOGGER as LGSTS1Q.cbl<br/>*(Error and Message Logger)*

  MENU->>INQUIRY: Dispatch policy inquiry request
  INQUIRY->>INQDB: Fetch policy data
  INQUIRY->>LOGGER: Log errors/messages
  MENU->>ADD: Dispatch policy addition request
  ADD->>ADDDB: Process policy addition
  ADD->>LOGGER: Log errors/messages
  MENU->>DEL: Dispatch policy deletion request
  DEL->>DELDB: Process policy deletion
  DELDB->>DELVSAM: Delete policy from VSAM file
  DEL->>LOGGER: Log errors/messages
  DELDB->>LOGGER: Log errors/messages
  DELVSAM->>LOGGER: Log errors/messages
  MENU->>UPD: Dispatch policy update request
  UPD->>UPDDB: Process policy update
  UPDDB->>UPDVSAM: Update VSAM policy file
  UPD->>LOGGER: Log errors/messages
  UPDDB->>LOGGER: Log errors/messages
  UPDVSAM->>LOGGER: Log errors/messages

%% Swimm:
%% sequenceDiagram
%%   participant MENU as LGTESTP1.cbl<br/>*(Motor Policy Menu Controller)*
%%   participant INQUIRY as LGIPOL01.cbl<br/>*(Policy Inquiry Dispatcher)*
%%   participant INQDB as LGIPDB01.cbl<br/>*(Policy Data Fetcher)*
%%   participant ADD as LGAPOL01.cbl<br/>*(Policy Addition Validator)*
%%   participant ADDDB as <SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath><br/>*(Policy Addition Processor)*
%%   participant DEL as LGDPOL01.cbl<br/>*(Policy Deletion Validator)*
%%   participant DELDB as LGDPDB01.cbl<br/>*(Policy Deletion Processor)*
%%   participant DELVSAM as LGDPVS01.cbl<br/>*(VSAM Policy File Deletion Handler)*
%%   participant UPD as LGUPOL01.cbl<br/>*(Policy Update Validator)*
%%   participant UPDDB as LGUPDB01.cbl<br/>*(Policy Update Processor)*
%%   participant UPDVSAM as LGUPVS01.cbl<br/>*(VSAM Policy File Update Handler)*
%%   participant LOGGER as LGSTS1Q.cbl<br/>*(Error and Message Logger)*
%% 
%%   MENU->>INQUIRY: Dispatch policy inquiry request
%%   INQUIRY->>INQDB: Fetch policy data
%%   INQUIRY->>LOGGER: Log errors/messages
%%   MENU->>ADD: Dispatch policy addition request
%%   ADD->>ADDDB: Process policy addition
%%   ADD->>LOGGER: Log errors/messages
%%   MENU->>DEL: Dispatch policy deletion request
%%   DEL->>DELDB: Process policy deletion
%%   DELDB->>DELVSAM: Delete policy from VSAM file
%%   DEL->>LOGGER: Log errors/messages
%%   DELDB->>LOGGER: Log errors/messages
%%   DELVSAM->>LOGGER: Log errors/messages
%%   MENU->>UPD: Dispatch policy update request
%%   UPD->>UPDDB: Process policy update
%%   UPDDB->>UPDVSAM: Update VSAM policy file
%%   UPD->>LOGGER: Log errors/messages
%%   UPDDB->>LOGGER: Log errors/messages
%%   UPDVSAM->>LOGGER: Log errors/messages
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
