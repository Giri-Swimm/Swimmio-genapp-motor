---
title: Risk Assessment and Premium Calculation (LGPCALC1) - Overview
---
# Overview

This document describes the flow for processing insurance application requests. Requests are routed to the appropriate calculation routines based on the action code, resulting in risk scores, risk status, and premium values as required.

```mermaid
flowchart TD
    node1["Dispatching Actions Based on Request Type
(Dispatching Actions Based on Request Type)"]:::HeadingStyle --> node2{"Action code?
(Dispatching Actions Based on Request Type)"}:::HeadingStyle
    click node1 goToHeading "Dispatching Actions Based on Request Type"
    node2 -->|"Risk Score Only"| node3["Adjusting Risk Score by Property Type"]:::HeadingStyle
    node2 -->|"Status Only"| node4["Classifying Risk Status"]:::HeadingStyle
    node2 -->|"Full Quote"| node3
    node3 --> node4

    click node2 goToHeading "Dispatching Actions Based on Request Type"
    click node3 goToHeading "Adjusting Risk Score by Property Type"
    click node4 goToHeading "Classifying Risk Status"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- LGPCALC1 (<SwmPath>[base/src/lgpcalc1.cbl](base/src/lgpcalc1.cbl)</SwmPath>)

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
