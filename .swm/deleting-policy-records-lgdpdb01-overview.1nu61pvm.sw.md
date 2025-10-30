---
title: Deleting Policy Records (LGDPDB01) - Overview
---
# Overview

This document explains the flow for deleting policy records from DB2 and VSAM. The process validates requests, deletes the relevant records, and logs error messages with transaction context for support and audit.

```mermaid
flowchart TD
  node1["Input Checks and Data Preparation"]:::HeadingStyle --> node2{"Is input sufficient and request type supported?"}
  click node1 goToHeading "Input Checks and Data Preparation"
  node2 -->|"No"| node5["Return error"]
  node2 -->|"Yes"| node3["DB2 Policy Deletion and Error Handling"]:::HeadingStyle
  click node3 goToHeading "DB2 Policy Deletion and Error Handling"
  node3 --> node4{"Did DB2 deletion succeed?"}
  node4 -->|"No"| node5
  node4 -->|"Yes"| node6["VSAM Policy Deletion and Error Reporting"]:::HeadingStyle
  click node6 goToHeading "VSAM Policy Deletion and Error Reporting"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Technical Overview

```mermaid
sequenceDiagram
  participant DB2 as LGDPDB01.cbl<br/>*(Handles DB2 policy deletion and coordinates the overall flow)*
  participant VSAM as LGDPVS01.cbl<br/>*(Handles VSAM policy deletion)*
  participant LOG as LGSTSQ.cbl<br/>*(Logs error messages and transaction context)*
  DB2->>LOG: Log error if input invalid or DB2 deletion fails
  DB2->>VSAM: Request VSAM policy deletion
  VSAM->>LOG: Log error if VSAM deletion fails
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
  8apwy("Deleting Policy Business Logic (LGDPOL01)") --> gw3vj("Deleting Policy Records (LGDPDB01)"):::currentEntity
click 8apwy openCode "base/src/lgdpol01.cbl:1"
  
  
click gw3vj openCode "base/src/lgdpdb01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   8apwy("Deleting Policy Business Logic (LGDPOL01)") --> gw3vj("Deleting Policy Records (LGDPDB01)"):::currentEntity
%% click 8apwy openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%%   
%%   
%% click gw3vj openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
