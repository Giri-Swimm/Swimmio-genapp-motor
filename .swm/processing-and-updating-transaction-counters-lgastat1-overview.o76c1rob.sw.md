---
title: Processing and Updating Transaction Counters (LGASTAT1) - Overview
---
# Overview

This document describes the flow for processing transaction requests. The flow receives transaction request data, extracts and normalizes business identifiers, manages queue state, and prepares the transaction context for further business processing.

```mermaid
flowchart TD
  node1["Processing Transaction Request
Receive request data
(Processing Transaction Request)"]:::HeadingStyle
  click node1 goToHeading "Processing Transaction Request"
  node1 --> node2{"Are identifiers available?
(Processing Transaction Request)"}:::HeadingStyle
  click node2 goToHeading "Processing Transaction Request"
  node2 -->|"Yes"| node3["Set business identifiers
(Processing Transaction Request)"]:::HeadingStyle
  click node3 goToHeading "Processing Transaction Request"
  node2 -->|"No"| node4{"Is commarea present?
(Processing Transaction Request)"}:::HeadingStyle
  click node4 goToHeading "Processing Transaction Request"
  node4 -->|"No"| node5["Return control to system
(Processing Transaction Request)"]:::HeadingStyle
  click node5 goToHeading "Processing Transaction Request"
  node4 -->|"Yes"| node3
  node3 --> node6["Ensure queue state
(Processing Transaction Request)"]:::HeadingStyle
  click node6 goToHeading "Processing Transaction Request"
  node6 --> node7["Apply business rules
(Normalize identifiers, flag special cases)
(Processing Transaction Request)"]:::HeadingStyle
  click node7 goToHeading "Processing Transaction Request"
  node7 --> node8["Prepare transaction context for further processing
(Processing Transaction Request)"]:::HeadingStyle
  click node8 goToHeading "Processing Transaction Request"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Copybook

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
