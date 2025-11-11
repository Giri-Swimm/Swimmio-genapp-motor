---
title: Processing Policy Data (LGAPOL01) - Overview
---
# Overview

This document explains the flow of processing insurance policy data. The system validates incoming records, calculates premiums for eligible commercial policies, applies business rules, and generates output records and summary statistics.

```mermaid
flowchart TD
    node1["Validating Input and Preparing for Processing"]:::HeadingStyle --> node2{"Is policy record valid?"}
    click node1 goToHeading "Validating Input and Preparing for Processing"
    node2 -->|"Valid"| node3{"Is policy commercial?"}
    node2 -->|"Invalid"| node6["Writing Error Records and Tracking Invalid Policies"]:::HeadingStyle
    click node6 goToHeading "Writing Error Records and Tracking Invalid Policies"
    node3 -->|"Yes"| node4["Calculating Premiums for Commercial Policies"]:::HeadingStyle
    click node4 goToHeading "Calculating Premiums for Commercial Policies"
    node3 -->|"No"| node6
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGAPOL01 (<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>)
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
  lvh0j("Motor Policy Menu (LGTESTP1)") --> zysqh("Processing Policy Data (LGAPOL01)"):::currentEntity
click lvh0j openCode "base/src/lgtestp1.cbl:1"
bjefb("House Policy Menu (LGTESTP3)") --> zysqh("Processing Policy Data (LGAPOL01)"):::currentEntity
click bjefb openCode "base/src/lgtestp3.cbl:1"
5iad0("Endowment Policy Menu (LGTESTP2)") --> zysqh("Processing Policy Data (LGAPOL01)"):::currentEntity
click 5iad0 openCode "base/src/lgtestp2.cbl:1"
7is6v("Managing Commercial Policies (LGTESTP4)") --> zysqh("Processing Policy Data (LGAPOL01)"):::currentEntity
click 7is6v openCode "base/src/lgtestp4.cbl:1"
  
  
click zysqh openCode "base/src/lgapol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   lvh0j("Motor Policy Menu (LGTESTP1)") --> zysqh("Processing Policy Data (LGAPOL01)"):::currentEntity
%% click lvh0j openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%% bjefb("House Policy Menu (LGTESTP3)") --> zysqh("Processing Policy Data (LGAPOL01)"):::currentEntity
%% click bjefb openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% 5iad0("Endowment Policy Menu (LGTESTP2)") --> zysqh("Processing Policy Data (LGAPOL01)"):::currentEntity
%% click 5iad0 openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% 7is6v("Managing Commercial Policies (LGTESTP4)") --> zysqh("Processing Policy Data (LGAPOL01)"):::currentEntity
%% click 7is6v openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%%   
%%   
%% click zysqh openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
