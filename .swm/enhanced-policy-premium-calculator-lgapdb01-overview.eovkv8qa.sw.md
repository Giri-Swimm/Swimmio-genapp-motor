---
title: Enhanced Policy Premium Calculator (LGAPDB01) - Overview
---
# Overview

This document describes the flow for processing insurance policy records, including validation, routing, risk assessment, premium calculation, and summary reporting. Commercial policies are evaluated for risk and may receive advanced actuarial premium calculations, while non-commercial policies are rejected. The flow produces calculated premiums, underwriting decisions, and summary statistics.

```mermaid
flowchart TD
  node1["Input Record Processing Loop"]:::HeadingStyle --> node2["Input Record Validation"]:::HeadingStyle
  click node1 goToHeading "Input Record Processing Loop"
  click node2 goToHeading "Input Record Validation"
  node2 --> node3{"Valid Record Routing and Processing
(Valid Record Routing and Processing)"}:::HeadingStyle
  click node3 goToHeading "Valid Record Routing and Processing"
  node3 -->|"Invalid or Non-commercial"| node6["Statistics Aggregation and Reporting"]:::HeadingStyle
  node3 -->|"Commercial"| node4["Commercial Policy Risk and Premium Calculation"]:::HeadingStyle
  click node4 goToHeading "Commercial Policy Risk and Premium Calculation"
  click node6 goToHeading "Statistics Aggregation and Reporting"
  node4 -->|"If eligible, perform advanced actuarial calculation"| node5["Business Rules, Output, and Statistics Update"]:::HeadingStyle
  click node5 goToHeading "Business Rules, Output, and Statistics Update"
  node5 --> node6

classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGAPDB01 (<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>)
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
  1gsew("Insurance Policy Premium Calculation Job (LGAPJOB)") --> c9a57("Enhanced Policy Premium Calculator (LGAPDB01)"):::currentEntity
click 1gsew openCode "base/cntl/lgapjob.jcl:1"
o0x84("Processing Policy Data (LGAPOL01)") --> c9a57("Enhanced Policy Premium Calculator (LGAPDB01)"):::currentEntity
click o0x84 openCode "base/src/lgapol01.cbl:1"
  
  
click c9a57 openCode "base/src/LGAPDB01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   1gsew("Insurance Policy Premium Calculation Job (LGAPJOB)") --> c9a57("Enhanced Policy Premium Calculator (LGAPDB01)"):::currentEntity
%% click 1gsew openCode "<SwmPath>[base/cntl/lgapjob.jcl](base/cntl/lgapjob.jcl)</SwmPath>:1"
%% o0x84("Processing Policy Data (LGAPOL01)") --> c9a57("Enhanced Policy Premium Calculator (LGAPDB01)"):::currentEntity
%% click o0x84 openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%   
%%   
%% click c9a57 openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
