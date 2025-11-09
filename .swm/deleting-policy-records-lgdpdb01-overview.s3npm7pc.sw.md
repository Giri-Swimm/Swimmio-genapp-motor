---
title: Deleting Policy Records (LGDPDB01) - Overview
---
# Overview

This document explains the flow for deleting policy records from both DB2 and VSAM storage. The process validates the incoming request, checks if the request type is supported, deletes the policy record from DB2, and then removes it from VSAM. Errors are logged and sent to storage queues for audit and troubleshooting.

```mermaid
flowchart TD
    node1["Validating commarea and prepping for DB2 operations
(Validating commarea and prepping for DB2 operations)"]:::HeadingStyle --> node2{"Is commarea large enough?"}
    click node1 goToHeading "Validating commarea and prepping for DB2 operations"
    node2 -->|"No"| node5["Return error
(Validating commarea and prepping for DB2 operations)"]:::HeadingStyle
    click node5 goToHeading "Validating commarea and prepping for DB2 operations"
    node2 -->|"Yes"| node3{"Is request type supported?"}
    node3 -->|"No"| node5
    node3 -->|"Yes"| node4["Deleting policy record in DB2"]:::HeadingStyle
    click node4 goToHeading "Deleting policy record in DB2"
    node4 --> node6{"Did DB2 deletion succeed?"}
    node6 -->|"No"| node5
    node6 -->|"Yes"| node7["Deleting policy record in VSAM and error handling"]:::HeadingStyle
    click node7 goToHeading "Deleting policy record in VSAM and error handling"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Technical Overview

```mermaid
sequenceDiagram
    participant LGDPDB01 as LGDPDB01.cbl<br/>*(Coordinates policy deletion and validation)*
    participant LGDPVS01 as LGDPVS01.cbl<br/>*(Deletes policy record from VSAM)*
    participant LGSTSQ as LGSTSQ.cbl<br/>*(Logs errors and sends messages to queues)*
    LGDPDB01->>LGDPVS01: Delegate VSAM policy record deletion
    LGDPDB01->>LGSTSQ: Send error messages and logs
```

## Dependencies

### Programs

- LGDPVS01 (<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)
- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  25gff("Deleting Policy Business Logic (LGDPOL01)") --> 9vz6n("Deleting Policy Records (LGDPDB01)"):::currentEntity
click 25gff openCode "base/src/lgdpol01.cbl:1"
  
  
click 9vz6n openCode "base/src/lgdpdb01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   25gff("Deleting Policy Business Logic (LGDPOL01)") --> 9vz6n("Deleting Policy Records (LGDPDB01)"):::currentEntity
%% click 25gff openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%%   
%%   
%% click 9vz6n openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
