---
title: Processing and Validating Policy Data (LGAPOL01) - Overview
---
# Overview

This document describes the flow for processing insurance policy requests. Incoming requests are validated, errors are logged and dispatched, and valid requests are routed to the premium calculation workflow. Premiums are calculated using risk factors and advanced actuarial logic, with business rules applied and statistics updated.

```mermaid
flowchart TD
  node1["Starting the Policy Processing Flow"]:::HeadingStyle
  node1 --> node2{"Is input valid and sufficient?
(Input Length Validation and Routing)"}:::HeadingStyle
  click node1 goToHeading "Starting the Policy Processing Flow"
  click node2 goToHeading "Input Length Validation and Routing"
  node2 -->|"No"| node3["Error Logging and Message Dispatch"]:::HeadingStyle
  click node3 goToHeading "Error Logging and Message Dispatch"
  node2 -->|"Yes"| node4["Premium Calculation Workflow Setup"]:::HeadingStyle
  click node4 goToHeading "Premium Calculation Workflow Setup"
  node4 --> node5{"Is record valid and commercial?
(Processing Validated Policy Records)"}:::HeadingStyle
  click node5 goToHeading "Processing Validated Policy Records"
  node5 -->|"No"| node6["Non-Commercial Policy or Error Handling
(Processing Validated Policy Records)"]:::HeadingStyle
  click node6 goToHeading "Processing Validated Policy Records"
  node5 -->|"Yes"| node7["Commercial Policy Actuarial Processing"]:::HeadingStyle
  click node7 goToHeading "Commercial Policy Actuarial Processing"
  node7 --> node8{"Is premium above threshold?
(Advanced Actuarial Calculation and Update)"}:::HeadingStyle
  click node8 goToHeading "Advanced Actuarial Calculation and Update"
  node8 -->|"No"| node9["Applying Business Rules, Output, and Statistics"]:::HeadingStyle
  click node9 goToHeading "Applying Business Rules, Output, and Statistics"
  node8 -->|"Yes"| node10["Advanced Actuarial Calculation and Update
(Advanced Actuarial Calculation and Update)"]:::HeadingStyle
  click node10 goToHeading "Advanced Actuarial Calculation and Update"
  node10 --> node9
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Technical Overview

```mermaid
sequenceDiagram
  participant POLICY as LGAPOL01.cbl<br/>*(Policy Request Processor)*
  participant MSG as LGSTSQ.cbl<br/>*(Error and Message Dispatcher)*
  participant PREMIUM as base/src/LGAPDB01.cbl<br/>*(Premium Calculation Workflow)*
  participant RISK as base/src/LGAPDB03.cbl<br/>*(Risk Factor and Basic Premium Calculator)*
  participant ACTUARY as base/src/LGAPDB04.cbl<br/>*(Advanced Actuarial Calculator)*

  POLICY->>MSG: Dispatch error/audit messages for invalid input
  POLICY->>PREMIUM: Route valid policy request for premium calculation
  PREMIUM->>RISK: Request risk factor lookup and basic premium calculation
  PREMIUM->>ACTUARY: Request advanced actuarial calculation (if eligible)
  PREMIUM-->>POLICY: Return processed policy record and statistics

%% Swimm:
%% sequenceDiagram
%%   participant POLICY as LGAPOL01.cbl<br/>*(Policy Request Processor)*
%%   participant MSG as LGSTSQ.cbl<br/>*(Error and Message Dispatcher)*
%%   participant PREMIUM as <SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath><br/>*(Premium Calculation Workflow)*
%%   participant RISK as <SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath><br/>*(Risk Factor and Basic Premium Calculator)*
%%   participant ACTUARY as <SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath><br/>*(Advanced Actuarial Calculator)*
%% 
%%   POLICY->>MSG: Dispatch error/audit messages for invalid input
%%   POLICY->>PREMIUM: Route valid policy request for premium calculation
%%   PREMIUM->>RISK: Request risk factor lookup and basic premium calculation
%%   PREMIUM->>ACTUARY: Request advanced actuarial calculation (if eligible)
%%   PREMIUM-->>POLICY: Return processed policy record and statistics
```

## Dependencies

### Programs

- LGAPDB01 (<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>)
- LGAPDB02
- LGAPDB03 (<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>)
- LGAPDB04 (<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- SQLCA
- INPUTREC2 (<SwmPath>[base/src/INPUTREC2.cpy](base/src/INPUTREC2.cpy)</SwmPath>)
- OUTPUTREC (<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>)
- WORKSTOR (<SwmPath>[base/src/WORKSTOR.cpy](base/src/WORKSTOR.cpy)</SwmPath>)
- LGAPACT (<SwmPath>[base/src/LGAPACT.cpy](base/src/LGAPACT.cpy)</SwmPath>)
- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  xd1qk("Managing Commercial Policy Operations (LGTESTP4)") --> 6p9e2("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity
click xd1qk openCode "base/src/lgtestp4.cbl:1"
xdeec("Endowment Policy Menu (LGTESTP2)") --> 6p9e2("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity
click xdeec openCode "base/src/lgtestp2.cbl:1"
g65s6("House Policy Menu (LGTESTP3)") --> 6p9e2("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity
click g65s6 openCode "base/src/lgtestp3.cbl:1"
cgqxo("Motor Policy Menu (LGTESTP1)") --> 6p9e2("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity
click cgqxo openCode "base/src/lgtestp1.cbl:1"
  
  
click 6p9e2 openCode "base/src/lgapol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   xd1qk("Managing Commercial Policy Operations (LGTESTP4)") --> 6p9e2("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity
%% click xd1qk openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%% xdeec("Endowment Policy Menu (LGTESTP2)") --> 6p9e2("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity
%% click xdeec openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% g65s6("House Policy Menu (LGTESTP3)") --> 6p9e2("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity
%% click g65s6 openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% cgqxo("Motor Policy Menu (LGTESTP1)") --> 6p9e2("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity
%% click cgqxo openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%%   
%%   
%% click 6p9e2 openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
