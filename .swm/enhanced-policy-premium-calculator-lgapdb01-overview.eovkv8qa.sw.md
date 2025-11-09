---
title: Enhanced Policy Premium Calculator (LGAPDB01) - Overview
---
# Overview

This document describes the flow for calculating enhanced insurance policy premiums. Insurance policy records are validated and routed for commercial or non-commercial processing. Commercial policies undergo risk assessment and premium calculation, with advanced actuarial logic applied when eligible. The flow aggregates statistics and writes output records for both valid and error cases.

```mermaid
flowchart TD
    node1["Input Record Processing"]:::HeadingStyle --> node2{"Valid Policy Routing
(Valid Policy Routing)"}:::HeadingStyle
    click node1 goToHeading "Input Record Processing"
    click node2 goToHeading "Valid Policy Routing"
    node2 -->|"Commercial"| node3["Commercial Policy Calculation"]:::HeadingStyle
    click node3 goToHeading "Commercial Policy Calculation"
    node3 --> node4["Business Rules, Output, and Statistics"]:::HeadingStyle
    click node4 goToHeading "Business Rules, Output, and Statistics"
    node4 --> node5["Statistics Aggregation"]:::HeadingStyle
    click node5 goToHeading "Statistics Aggregation"
    node2 -->|"Non-commercial or error"| node5
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Technical Overview

```mermaid
sequenceDiagram
    participant MAIN as base/src/LGAPDB01.cbl<br/>*(Enhanced Policy Premium Calculator)*
    participant RISKPREM as base/src/LGAPDB03.cbl<br/>*(Risk Factor and Basic Premium Computation)*
    participant ACTUARIAL as base/src/LGAPDB04.cbl<br/>*(Advanced Actuarial Premium Calculation)*
    MAIN->>RISKPREM: Request risk factors and basic premium computation
    RISKPREM-->>MAIN: Return risk factors and basic premium
    MAIN->>ACTUARIAL: Trigger advanced actuarial premium calculation (if eligible)
    ACTUARIAL-->>MAIN: Return enhanced premium calculation

%% Swimm:
%% sequenceDiagram
%%     participant MAIN as <SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath><br/>*(Enhanced Policy Premium Calculator)*
%%     participant RISKPREM as <SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath><br/>*(Risk Factor and Basic Premium Computation)*
%%     participant ACTUARIAL as <SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath><br/>*(Advanced Actuarial Premium Calculation)*
%%     MAIN->>RISKPREM: Request risk factors and basic premium computation
%%     RISKPREM-->>MAIN: Return risk factors and basic premium
%%     MAIN->>ACTUARIAL: Trigger advanced actuarial premium calculation (if eligible)
%%     ACTUARIAL-->>MAIN: Return enhanced premium calculation
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
  ajr3x("Insurance Policy Premium Calculation Job (LGAPJOB)") --> g0gbd("Enhanced Policy Premium Calculator (LGAPDB01)"):::currentEntity
click ajr3x openCode "base/cntl/lgapjob.jcl:1"
6u2f8("Processing Policy Data (LGAPOL01)") --> g0gbd("Enhanced Policy Premium Calculator (LGAPDB01)"):::currentEntity
click 6u2f8 openCode "base/src/lgapol01.cbl:1"
  
  
click g0gbd openCode "base/src/LGAPDB01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   ajr3x("Insurance Policy Premium Calculation Job (LGAPJOB)") --> g0gbd("Enhanced Policy Premium Calculator (LGAPDB01)"):::currentEntity
%% click ajr3x openCode "<SwmPath>[base/cntl/lgapjob.jcl](base/cntl/lgapjob.jcl)</SwmPath>:1"
%% 6u2f8("Processing Policy Data (LGAPOL01)") --> g0gbd("Enhanced Policy Premium Calculator (LGAPDB01)"):::currentEntity
%% click 6u2f8 openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%   
%%   
%% click g0gbd openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
