---
title: Processing Policy Data (LGAPOL01) - Overview
---
# Overview

This document describes the flow of processing policy requests, including validation, error logging, record preparation, premium calculation, and underwriting. The flow coordinates multiple programs to ensure only valid commercial policies are processed and business rules are applied.

```mermaid
flowchart TD
  node1["Starting Policy Processing"]:::HeadingStyle --> node2["Validating and Preparing Policy Data"]:::HeadingStyle
  node2 --> node3["Coordinating Policy File Processing"]:::HeadingStyle
  node3 --> node4["Processing and Validating Policy Records"]:::HeadingStyle
  node4 --> node5["Calculating and Underwriting Commercial Policies"]:::HeadingStyle
  node5 --> node6["Running Basic Premium Calculation"]:::HeadingStyle
  node6 --> node7{"Enhanced Actuarial Calculation?
(Running Enhanced Actuarial Calculation)"}:::HeadingStyle
  node7 -->|"Yes"| node8["Calculating Premium Components and Finalizing"]:::HeadingStyle
  node7 -->|"No"| node8
  node8 --> node9["Applying Underwriting Decisions and Updating Stats"]:::HeadingStyle

  click node1 goToHeading "Starting Policy Processing"
  click node2 goToHeading "Validating and Preparing Policy Data"
  click node3 goToHeading "Coordinating Policy File Processing"
  click node4 goToHeading "Processing and Validating Policy Records"
  click node5 goToHeading "Calculating and Underwriting Commercial Policies"
  click node6 goToHeading "Running Basic Premium Calculation"
  click node6 goToHeading "Risk Factor Lookup and Premium Math"
  click node7 goToHeading "Running Enhanced Actuarial Calculation"
  click node8 goToHeading "Calculating Premium Components and Finalizing"
  click node9 goToHeading "Applying Underwriting Decisions and Updating Stats"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Technical Overview

```mermaid
sequenceDiagram
  participant POLICY as LGAPOL01.cbl<br/>*(Policy Processor)*
  participant LOGGER as LGSTSQ.cbl<br/>*(Error Logger and Message Router)*
  participant FILE as base/src/LGAPDB01.cbl<br/>*(Policy File Coordinator and Record Processor)*
  participant RISK as base/src/LGAPDB03.cbl<br/>*(Risk Factor and Premium Calculator)*
  participant ACTUARY as base/src/LGAPDB04.cbl<br/>*(Advanced Actuarial Calculator)*

  POLICY->>LOGGER: Log errors and route messages
  POLICY->>FILE: Coordinate policy file processing and record validation
  FILE->>RISK: Calculate risk factors and basic premiums
  FILE->>ACTUARY: Perform enhanced actuarial calculations (if eligible)
  ACTUARY-->>FILE: Return enhanced premium results
  RISK-->>FILE: Return risk and premium results
  FILE-->>POLICY: Return processed policy records and statistics

%% Swimm:
%% sequenceDiagram
%%   participant POLICY as LGAPOL01.cbl<br/>*(Policy Processor)*
%%   participant LOGGER as LGSTSQ.cbl<br/>*(Error Logger and Message Router)*
%%   participant FILE as <SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath><br/>*(Policy File Coordinator and Record Processor)*
%%   participant RISK as <SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath><br/>*(Risk Factor and Premium Calculator)*
%%   participant ACTUARY as <SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath><br/>*(Advanced Actuarial Calculator)*
%% 
%%   POLICY->>LOGGER: Log errors and route messages
%%   POLICY->>FILE: Coordinate policy file processing and record validation
%%   FILE->>RISK: Calculate risk factors and basic premiums
%%   FILE->>ACTUARY: Perform enhanced actuarial calculations (if eligible)
%%   ACTUARY-->>FILE: Return enhanced premium results
%%   RISK-->>FILE: Return risk and premium results
%%   FILE-->>POLICY: Return processed policy records and statistics
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
  4bv86("Motor Policy Menu (LGTESTP1)") --> qioxb("Processing Policy Data (LGAPOL01)"):::currentEntity
click 4bv86 openCode "base/src/lgtestp1.cbl:1"
b0wbx("House Policy Menu (LGTESTP3)") --> qioxb("Processing Policy Data (LGAPOL01)"):::currentEntity
click b0wbx openCode "base/src/lgtestp3.cbl:1"
4o7md("Endowment Policy Menu (LGTESTP2)") --> qioxb("Processing Policy Data (LGAPOL01)"):::currentEntity
click 4o7md openCode "base/src/lgtestp2.cbl:1"
6g7ia("Managing Commercial Policies (LGTESTP4)") --> qioxb("Processing Policy Data (LGAPOL01)"):::currentEntity
click 6g7ia openCode "base/src/lgtestp4.cbl:1"
  
  
click qioxb openCode "base/src/lgapol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   4bv86("Motor Policy Menu (LGTESTP1)") --> qioxb("Processing Policy Data (LGAPOL01)"):::currentEntity
%% click 4bv86 openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%% b0wbx("House Policy Menu (LGTESTP3)") --> qioxb("Processing Policy Data (LGAPOL01)"):::currentEntity
%% click b0wbx openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% 4o7md("Endowment Policy Menu (LGTESTP2)") --> qioxb("Processing Policy Data (LGAPOL01)"):::currentEntity
%% click 4o7md openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% 6g7ia("Managing Commercial Policies (LGTESTP4)") --> qioxb("Processing Policy Data (LGAPOL01)"):::currentEntity
%% click 6g7ia openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%%   
%%   
%% click qioxb openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
