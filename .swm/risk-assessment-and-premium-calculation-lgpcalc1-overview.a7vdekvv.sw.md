---
title: Risk Assessment and Premium Calculation (LGPCALC1) - Overview
---
# Overview

This document describes the flow for processing insurance applications. The system evaluates the requested operation and executes the relevant business logic to calculate risk scores, assign risk status, and determine premiums.

```mermaid
flowchart TD
    node1["Controlling the Calculation Flow
(Controlling the Calculation Flow)"]:::HeadingStyle --> node2{"Requested Operation?
(Controlling the Calculation Flow)"}:::HeadingStyle
    click node1 goToHeading "Controlling the Calculation Flow"
    click node2 goToHeading "Controlling the Calculation Flow"
    node2 -->|"Risk Score"| node3["Calculating the Risk Score"]:::HeadingStyle
    click node3 goToHeading "Calculating the Risk Score"
    node2 -->|"Status"| node4["Assigning Risk Status and Reason"]:::HeadingStyle
    click node4 goToHeading "Assigning Risk Status and Reason"
    node2 -->|"Premium or All"| node3
    node3 --> node4
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- LGPCALC1 (<SwmPath>[base/src/lgpcalc1.cbl](base/src/lgpcalc1.cbl)</SwmPath>)

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
