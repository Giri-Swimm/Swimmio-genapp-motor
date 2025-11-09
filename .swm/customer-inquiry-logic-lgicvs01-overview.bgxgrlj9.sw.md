---
title: Customer Inquiry Logic (LGICVS01) - Overview
---
# Overview

This flow returns a random customer number from the customer dataset. It manages customer number boundaries through queue messages and delivers the result as a screen message or data structure, depending on how the transaction was started.

```mermaid
flowchart TD
  node1["Startup and Input Assignment
Determine transaction start type and assign input
(Startup and Input Assignment)"]:::HeadingStyle
  click node1 goToHeading "Startup and Input Assignment"
  node1 --> node2["Manage customer boundaries via queue messages
(Startup and Input Assignment)"]:::HeadingStyle
  click node2 goToHeading "Startup and Input Assignment"
  node2 --> node3["Generate random customer number within boundaries
(Startup and Input Assignment)"]:::HeadingStyle
  click node3 goToHeading "Startup and Input Assignment"
  node3 --> node4["Prepare output
(Startup and Input Assignment)"]:::HeadingStyle
  click node4 goToHeading "Startup and Input Assignment"
  node4 --> node5{"Output as screen message or data structure?
(Startup and Input Assignment)"}:::HeadingStyle
  click node5 goToHeading "Startup and Input Assignment"
  node5 -->|"Screen message"| node6["Send output as screen message
(Startup and Input Assignment)"]:::HeadingStyle
  click node6 goToHeading "Startup and Input Assignment"
  node5 -->|"Data structure"| node7["Return output in data structure
(Startup and Input Assignment)"]:::HeadingStyle
  click node7 goToHeading "Startup and Input Assignment"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
