---
title: Inquire Policy (LGIPVS01) - Overview
---
# Overview

This document describes how transaction requests are processed to inquire about policy information. The LGIPVS01 program determines the request source, validates policy data, and delivers a response message to the user or another program.

```mermaid
flowchart TD
    node1["Processing the Transaction Entry Point
Determine request source (direct/program or terminal)
(Processing the Transaction Entry Point)"]:::HeadingStyle --> node2{"Is policy type valid and read successful?"}
    click node1 goToHeading "Processing the Transaction Entry Point"
    node2 -->|"Prepare success message"| node3{"Response mode: 'R' or 'C'?"}
    node2 -->|"Prepare error message"| node3
    node3 -->|"'R'"| node4["Send message to user terminal
(Processing the Transaction Entry Point)"]:::HeadingStyle
    node3 -->|"'C'"| node5["Store message for program use
(Processing the Transaction Entry Point)"]:::HeadingStyle
    click node4 goToHeading "Processing the Transaction Entry Point"
    click node5 goToHeading "Processing the Transaction Entry Point"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- LGIPVS01 (<SwmPath>[base/src/lgipvs01.cbl](base/src/lgipvs01.cbl)</SwmPath>)

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
