---
title: Inserting Insurance Policy Data (LGAPDB09) - Overview
---
# Overview

This document describes the flow for processing insurance policy requests. The system validates input, determines the policy type, records policy data, and performs risk and premium calculations for commercial policies before storing the results.

```mermaid
flowchart TD
    node1["Initializing Transaction Context"]:::HeadingStyle
    click node1 goToHeading "Initializing Transaction Context"
    node1 --> node2["Preparing Policy Data for Processing"]:::HeadingStyle
    click node2 goToHeading "Preparing Policy Data for Processing"
    node2 --> node3["Inserting Main Policy Record"]:::HeadingStyle
    click node3 goToHeading "Inserting Main Policy Record"
    node3 --> node4{"Policy Type?
(Routing to Policy-Type Specific Insert)"}:::HeadingStyle
    click node4 goToHeading "Routing to Policy-Type Specific Insert"
    node4 -->|"Commercial"| node5["Calculating and Inserting Commercial Policy Details"]:::HeadingStyle
    click node5 goToHeading "Calculating and Inserting Commercial Policy Details"
    node4 -->|"Endowment, House, or Motor"| node6["Inserting Endowment, House, or Motor Policy Details
(Routing to Policy-Type Specific Insert)"]:::HeadingStyle
    click node6 goToHeading "Routing to Policy-Type Specific Insert"
    node5 --> node7["Formatting and Writing Policy Data"]:::HeadingStyle
    click node7 goToHeading "Formatting and Writing Policy Data"
    node6 --> node7
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGAPDB09 (<SwmPath>[base/src/lgapdb09.cbl](base/src/lgapdb09.cbl)</SwmPath>)
- LGAPVS01 (<SwmPath>[base/src/lgapvs01.cbl](base/src/lgapvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)
- LGCOMCAL (<SwmPath>[base/src/lgcomcal.cbl](base/src/lgcomcal.cbl)</SwmPath>)

### Copybooks

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)
- LGCOMDAT (<SwmPath>[base/src/lgcomdat.cpy](base/src/lgcomdat.cpy)</SwmPath>)
- LGPOLICY (<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>)
- SQLCA

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
