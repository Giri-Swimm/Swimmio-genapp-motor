---
title: Motor Policy Menu (LGTESTP1) - Overview
---
# Overview

This document explains the flow for managing motor insurance policies via a menu-driven interface. Users can inquire about, add, delete, or update motor policy records. The flow coordinates user interactions, validates requests, and delegates business operations to backend services.

```mermaid
flowchart TD
  node1["Initial Request Handling"]:::HeadingStyle --> node2["User Request Processing"]:::HeadingStyle
  click node1 goToHeading "Initial Request Handling"
  click node2 goToHeading "User Request Processing"
  node2 --> node3{"Which motor policy action?"}
  node3 -->|"Inquiry"| node4["Policy Inquiry Dispatch"]:::HeadingStyle
  click node4 goToHeading "Policy Inquiry Dispatch"
  node3 -->|"Add"| node5["Policy Application Validation"]:::HeadingStyle
  click node5 goToHeading "Policy Application Validation"
  node3 -->|"Delete"| node6["Validating and Routing Policy Deletion Requests"]:::HeadingStyle
  click node6 goToHeading "Validating and Routing Policy Deletion Requests"
  node3 -->|"Update"| node7["Validating and Routing Policy Update Requests"]:::HeadingStyle
  click node7 goToHeading "Validating and Routing Policy Update Requests"
  node4 --> node8["Display Policy Details"]:::HeadingStyle
  click node8 goToHeading "Display Policy Details"
  node5 --> node9["Show result or error to user
(then return to menu)"]
  node6 --> node9
  node7 --> node9
  node9 --> node2
  %% Arrow from node9 to node2 labeled 'Return to menu'
  node9 -. "Return to menu" .-> node2
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Technical Overview

```mermaid
sequenceDiagram
  participant MENU as LGTESTP1.cbl<br/>*(Motor Policy Menu Handler)*
  participant INQUIRY as LGIPOL01.cbl<br/>*(Policy Inquiry Dispatcher)*
  participant RETRIEVE as LGIPDB01.cbl<br/>*(Policy Data Retriever)*
  participant LOGGER as LGSTS1Q.cbl<br/>*(Error and Audit Logger)*
  participant ADDVAL as LGAPOL01.cbl<br/>*(Policy Application Validator)*
  participant ADDPROC as base/src/LGAPDB01.cbl<br/>*(Premium Calculator and Policy Inserter)*
  participant BASICPREM as base/src/LGAPDB03.cbl<br/>*(Basic Premium Calculator)*
  participant ADVPREM as base/src/LGAPDB04.cbl<br/>*(Advanced Actuarial Calculator)*
  participant DELVAL as LGDPOL01.cbl<br/>*(Policy Deletion Validator)*
  participant DELPROC as LGDPDB01.cbl<br/>*(Policy Deletion Processor)*
  participant DELVSAM as LGDPVS01.cbl<br/>*(VSAM Policy Deletion Handler)*
  participant UPDVAL as LGUPOL01.cbl<br/>*(Policy Update Validator)*
  participant UPDPROC as LGUPDB01.cbl<br/>*(Policy Update Processor)*
  participant UPDVSAM as LGUPVS01.cbl<br/>*(VSAM Policy Update Handler)*

  MENU->>INQUIRY: Route inquiry request
  INQUIRY->>RETRIEVE: Retrieve policy data
  INQUIRY->>LOGGER: Log errors/diagnostics

  MENU->>ADDVAL: Route add request
  ADDVAL->>ADDPROC: Insert policy and calculate premium
  ADDPROC->>BASICPREM: Basic premium calculation
  ADDPROC->>ADVPREM: Advanced actuarial calculation

  MENU->>DELVAL: Route delete request
  DELVAL->>DELPROC: Delete policy
  DELPROC->>DELVASAM: Delete policy from VSAM

  MENU->>UPDVAL: Route update request
  UPDVAL->>UPDPROC: Update policy
  UPDPROC->>UPDVSAM: Update policy in VSAM

%% Swimm:
%% sequenceDiagram
%%   participant MENU as LGTESTP1.cbl<br/>*(Motor Policy Menu Handler)*
%%   participant INQUIRY as LGIPOL01.cbl<br/>*(Policy Inquiry Dispatcher)*
%%   participant RETRIEVE as LGIPDB01.cbl<br/>*(Policy Data Retriever)*
%%   participant LOGGER as LGSTS1Q.cbl<br/>*(Error and Audit Logger)*
%%   participant ADDVAL as LGAPOL01.cbl<br/>*(Policy Application Validator)*
%%   participant ADDPROC as <SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath><br/>*(Premium Calculator and Policy Inserter)*
%%   participant BASICPREM as <SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath><br/>*(Basic Premium Calculator)*
%%   participant ADVPREM as <SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath><br/>*(Advanced Actuarial Calculator)*
%%   participant DELVAL as LGDPOL01.cbl<br/>*(Policy Deletion Validator)*
%%   participant DELPROC as LGDPDB01.cbl<br/>*(Policy Deletion Processor)*
%%   participant DELVSAM as LGDPVS01.cbl<br/>*(VSAM Policy Deletion Handler)*
%%   participant UPDVAL as LGUPOL01.cbl<br/>*(Policy Update Validator)*
%%   participant UPDPROC as LGUPDB01.cbl<br/>*(Policy Update Processor)*
%%   participant UPDVSAM as LGUPVS01.cbl<br/>*(VSAM Policy Update Handler)*
%% 
%%   MENU->>INQUIRY: Route inquiry request
%%   INQUIRY->>RETRIEVE: Retrieve policy data
%%   INQUIRY->>LOGGER: Log errors/diagnostics
%% 
%%   MENU->>ADDVAL: Route add request
%%   ADDVAL->>ADDPROC: Insert policy and calculate premium
%%   ADDPROC->>BASICPREM: Basic premium calculation
%%   ADDPROC->>ADVPREM: Advanced actuarial calculation
%% 
%%   MENU->>DELVAL: Route delete request
%%   DELVAL->>DELPROC: Delete policy
%%   DELPROC->>DELVASAM: Delete policy from VSAM
%% 
%%   MENU->>UPDVAL: Route update request
%%   UPDVAL->>UPDPROC: Update policy
%%   UPDPROC->>UPDVSAM: Update policy in VSAM
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
