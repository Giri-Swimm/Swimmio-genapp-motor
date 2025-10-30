---
title: Calculating Enhanced Insurance Premiums (LGAPDB01) - Overview
---
# Overview

This document explains the flow for calculating commercial insurance premiums and determining underwriting decisions for policy applications. Policy records are validated and routed, commercial policies are assessed for risk and premium, and business rules are applied to finalize decisions and update aggregate statistics.

```mermaid
flowchart TD
  node1["Input Record Processing"]:::HeadingStyle
  click node1 goToHeading "Input Record Processing"
  node1 --> node2{"Commercial Policy Routing
(Commercial Policy Routing)"}:::HeadingStyle
  click node2 goToHeading "Commercial Policy Routing"
  node2 -->|"Commercial"| node3["Commercial Premium Calculation"]:::HeadingStyle
  click node3 goToHeading "Commercial Premium Calculation"
  node3 --> node4{"Enhanced Actuarial Calculation Trigger
(Enhanced Actuarial Calculation Trigger)"}:::HeadingStyle
  click node4 goToHeading "Enhanced Actuarial Calculation Trigger"
  node4 -->|"Approved"| node5["Advanced Premium Component Calculation"]:::HeadingStyle
  click node5 goToHeading "Advanced Premium Component Calculation"
  node5 --> node6["Business Rule Application and Output"]:::HeadingStyle
  click node6 goToHeading "Business Rule Application and Output"
  node6 --> node7["Updating Aggregate Policy Statistics"]:::HeadingStyle
  click node7 goToHeading "Updating Aggregate Policy Statistics"
  node4 -->|"Not Approved"| node6
  node2 -->|"Non-Commercial"| node6
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Technical Overview

```mermaid
sequenceDiagram
  participant MAIN as base/src/LGAPDB01.cbl<br/>*(Batch Processor and Premium Calculator)*
  participant BASIC as base/src/LGAPDB03.cbl<br/>*(Risk Factor and Basic Premium Calculator)*
  participant ADV as base/src/LGAPDB04.cbl<br/>*(Advanced Actuarial Premium Calculator)*
  MAIN->>BASIC: Request risk factor and basic premium calculation
  BASIC-->>MAIN: Return risk factors and basic premium
  MAIN->>ADV: Request advanced actuarial premium calculation (if approved)
  ADV-->>MAIN: Return advanced premium components

%% Swimm:
%% sequenceDiagram
%%   participant MAIN as <SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath><br/>*(Batch Processor and Premium Calculator)*
%%   participant BASIC as <SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath><br/>*(Risk Factor and Basic Premium Calculator)*
%%   participant ADV as <SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath><br/>*(Advanced Actuarial Premium Calculator)*
%%   MAIN->>BASIC: Request risk factor and basic premium calculation
%%   BASIC-->>MAIN: Return risk factors and basic premium
%%   MAIN->>ADV: Request advanced actuarial premium calculation (if approved)
%%   ADV-->>MAIN: Return advanced premium components
```

## Dependencies

### Programs

- LGAPDB02
- LGAPDB03 (<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>)
- LGAPDB04 (<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>)

### Copybooks

- SQLCA
- INPUTREC2 (<SwmPath>[base/src/INPUTREC2.cpy](base/src/INPUTREC2.cpy)</SwmPath>)
- OUTPUTREC (<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>)
- WORKSTOR (<SwmPath>[base/src/WORKSTOR.cpy](base/src/WORKSTOR.cpy)</SwmPath>)
- LGAPACT (<SwmPath>[base/src/LGAPACT.cpy](base/src/LGAPACT.cpy)</SwmPath>)

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  4knvt("LGAPJOB") --> 3oc6l("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity
click 4knvt openCode "base/cntl/lgapjob.jcl:1"
v5jz7("Processing and Validating Policy Data (LGAPOL01)") --> 3oc6l("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity
click v5jz7 openCode "base/src/lgapol01.cbl:1"
  
  
click 3oc6l openCode "base/src/LGAPDB01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   4knvt("LGAPJOB") --> 3oc6l("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity
%% click 4knvt openCode "<SwmPath>[base/cntl/lgapjob.jcl](base/cntl/lgapjob.jcl)</SwmPath>:1"
%% v5jz7("Processing and Validating Policy Data (LGAPOL01)") --> 3oc6l("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity
%% click v5jz7 openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%   
%%   
%% click 3oc6l openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
