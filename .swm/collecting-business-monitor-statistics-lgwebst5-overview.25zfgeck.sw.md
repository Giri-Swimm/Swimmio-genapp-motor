---
title: Collecting Business Monitor Statistics (LGWEBST5) - Overview
---
# Overview

This document explains the flow of transaction statistics collection for business monitoring. The process captures transaction context, calculates elapsed intervals, aggregates counter values, and stores summary metrics in shared queues, refreshing every 60 seconds.

```mermaid
flowchart TD
    node1["Starting the Statistics Collection"]:::HeadingStyle
    click node1 goToHeading "Starting the Statistics Collection"
    node1 --> node2["Calculating the Elapsed Interval"]:::HeadingStyle
    click node2 goToHeading "Calculating the Elapsed Interval"
    node2 --> node3{"Was previous event time found?"}
    node3 -->|"Yes"| node4["Aggregating and Storing Counter Values"]:::HeadingStyle
    click node4 goToHeading "Aggregating and Storing Counter Values"
    node3 -->|"No"| node4
    node4 --> node1["Starting the Statistics Collection"]:::HeadingStyle
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- LGWEBST5 (<SwmPath>[base/src/lgwebst5.cbl](base/src/lgwebst5.cbl)</SwmPath>)

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
