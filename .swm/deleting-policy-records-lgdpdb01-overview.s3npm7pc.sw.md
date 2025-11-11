---
title: Deleting Policy Records (LGDPDB01) - Overview
---
# Overview

This document describes the flow for deleting a policy record for a customer. The process validates requests, deletes the policy from both DB2 and VSAM databases, and logs error context if deletion fails.

```mermaid
flowchart TD
    node1["Validating and Preparing Request Data
(Validating and Preparing Request Data)"]:::HeadingStyle --> node2{"Is request valid and supported?
(Validating and Preparing Request Data)"}:::HeadingStyle
    node2 -->|"No"|node5["Return result (error or success code)
(Deleting Policy from VSAM File)"]:::HeadingStyle
    node2 -->|"Yes"|node3["Deleting Policy from Database
(Deleting Policy from Database)"]:::HeadingStyle
    node3 --> node4{"Was DB2 deletion successful?
(Deleting Policy from Database)"}:::HeadingStyle
    node4 -->|"No"|node5
    node4 -->|"Yes"|node6["Deleting Policy from VSAM File
(Deleting Policy from VSAM File)"]:::HeadingStyle
    node6 --> node7{"Was VSAM deletion successful?
(Deleting Policy from VSAM File)"}:::HeadingStyle
    node7 -->|"No"|node5
    node7 -->|"Yes"|node5

    click node1 goToHeading "Validating and Preparing Request Data"
    click node2 goToHeading "Validating and Preparing Request Data"
    click node3 goToHeading "Deleting Policy from Database"
    click node4 goToHeading "Deleting Policy from Database"
    click node6 goToHeading "Deleting Policy from VSAM File"
    click node7 goToHeading "Deleting Policy from VSAM File"
    click node5 goToHeading "Deleting Policy from VSAM File"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGDPDB01 (<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>)
- LGDPVS01 (<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)
- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  ckqll("Deleting Policy Business Logic (LGDPOL01)") --> alw2p("Deleting Policy Records (LGDPDB01)"):::currentEntity
click ckqll openCode "base/src/lgdpol01.cbl:1"
  
  
click alw2p openCode "base/src/lgdpdb01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   ckqll("Deleting Policy Business Logic (LGDPOL01)") --> alw2p("Deleting Policy Records (LGDPDB01)"):::currentEntity
%% click ckqll openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%%   
%%   
%% click alw2p openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
