---
title: Risk Assessment and Premium Calculation (LGPCALC1) - Overview
---
# Overview

This document explains the flow for processing insurance calculations. Requests are routed to the relevant business logic to calculate risk scores, assign insurance status, and provide explanatory reasons based on property and location data.

```mermaid
flowchart TD
  node1["Entry and Action Dispatch
(Entry and Action Dispatch)"]:::HeadingStyle --> node2{"Requested operation?
(Entry and Action Dispatch)"}:::HeadingStyle
  node2 -->|"Risk"| node3["Property and Location Risk Adjustments"]:::HeadingStyle
  node3 --> node4["Risk Status Assignment"]:::HeadingStyle
  node2 -->|"Status"| node4
  node2 -->|"All"| node3
  node3 --> node4
  click node1 goToHeading "Entry and Action Dispatch"
  click node2 goToHeading "Entry and Action Dispatch"
  click node3 goToHeading "Property and Location Risk Adjustments"
  click node4 goToHeading "Risk Status Assignment"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
