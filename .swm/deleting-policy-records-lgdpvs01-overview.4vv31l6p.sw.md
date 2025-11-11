---
title: Deleting Policy Records (LGDPVS01) - Overview
---
# Overview

This document explains the flow of deleting policy records. The process receives a deletion request with customer and policy identifiers, attempts to remove the record, and logs error details with business context if the deletion fails.

```mermaid
flowchart TD
    node1["Starting Policy Deletion and Preparing Keys
(Starting Policy Deletion and Preparing Keys)"]:::HeadingStyle --> node2{"Was deletion successful?
(Starting Policy Deletion and Preparing Keys)"}:::HeadingStyle
    node2 -->|"Yes"| node3["No further action"]
    node2 -->|"No"| node4["Formatting and Logging Deletion Errors
(Formatting and Logging Deletion Errors)"]:::HeadingStyle
    node4 --> node5{"Is there additional error context data?
(Formatting and Logging Deletion Errors)"}:::HeadingStyle
    node5 -->|"Yes"| node6["Log additional error context"]
    node5 -->|"No"| node3
    node6 --> node3
    click node1 goToHeading "Starting Policy Deletion and Preparing Keys"
    click node2 goToHeading "Starting Policy Deletion and Preparing Keys"
    click node4 goToHeading "Formatting and Logging Deletion Errors"
    click node5 goToHeading "Formatting and Logging Deletion Errors"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGDPVS01 (<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybook

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  ry6ru("Deleting Policy Records (LGDPDB01)") --> 2be44("Deleting Policy Records (LGDPVS01)"):::currentEntity
click ry6ru openCode "base/src/lgdpdb01.cbl:1"
  
  
click 2be44 openCode "base/src/lgdpvs01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   ry6ru("Deleting Policy Records (LGDPDB01)") --> 2be44("Deleting Policy Records (LGDPVS01)"):::currentEntity
%% click ry6ru openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%   
%%   
%% click 2be44 openCode "<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
