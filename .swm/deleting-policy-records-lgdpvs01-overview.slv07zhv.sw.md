---
title: Deleting Policy Records (LGDPVS01) - Overview
---
# Overview

This document explains the flow of deleting a policy record and logging any errors encountered during the process. When a request is received with a customer and policy number, the system attempts to remove the corresponding policy record. If the deletion fails, detailed error information is captured and logged for audit and troubleshooting.

```mermaid
flowchart TD
    node1["Policy Deletion and Preparation
(Policy Deletion and Preparation)"]:::HeadingStyle --> node2{"Was deletion successful?
(Policy Deletion and Preparation)"}:::HeadingStyle
    node2 -->|"Yes"| node4["Flow ends
(Policy Deletion and Preparation)"]:::HeadingStyle
    node2 -->|"No"| node3["Error Message Generation and Logging"]:::HeadingStyle
    node3 --> node4
    click node1 goToHeading "Policy Deletion and Preparation"
    click node2 goToHeading "Policy Deletion and Preparation"
    click node3 goToHeading "Error Message Generation and Logging"
    click node4 goToHeading "Policy Deletion and Preparation"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Technical Overview

```mermaid
sequenceDiagram
  participant MAIN as LGDPVS01.cbl<br/>*(Policy Deletion Handler)*
  participant LOG as LGSTSQ.cbl<br/>*(Error Logging Handler)*
  MAIN->>MAIN: Attempt to delete policy record
  alt Deletion fails
    MAIN->>LOG: Log error details
  end
```

## Dependencies

### Program

- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybook

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  esgwk("Deleting Policy Records (LGDPDB01)") --> vao0n("Deleting Policy Records (LGDPVS01)"):::currentEntity
click esgwk openCode "base/src/lgdpdb01.cbl:1"
  
  
click vao0n openCode "base/src/lgdpvs01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   esgwk("Deleting Policy Records (LGDPDB01)") --> vao0n("Deleting Policy Records (LGDPVS01)"):::currentEntity
%% click esgwk openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%   
%%   
%% click vao0n openCode "<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
