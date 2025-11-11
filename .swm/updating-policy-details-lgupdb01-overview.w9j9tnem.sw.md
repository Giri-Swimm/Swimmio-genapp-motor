---
title: Updating Policy details (LGUPDB01) - Overview
---
# Overview

This document describes the flow for updating insurance policy details. Policy update requests are processed to update records in DB2 and VSAM databases, with data consistency, concurrency control, and comprehensive error logging.

```mermaid
flowchart TD
    node1["Preparing policy and customer data for DB2 update"]:::HeadingStyle --> node2["Updating DB2 policy record and handling concurrency
(Updating DB2 policy record and handling concurrency)"]:::HeadingStyle
    click node1 goToHeading "Preparing policy and customer data for DB2 update"
    click node2 goToHeading "Updating DB2 policy record and handling concurrency"
    node2 --> node3{"Timestamp matches database?
(Updating DB2 policy record and handling concurrency)"}:::HeadingStyle
    click node3 goToHeading "Updating DB2 policy record and handling concurrency"
    node3 -->|"Yes"| node4["Updating policy details in DB2
(Updating DB2 policy record and handling concurrency)"]:::HeadingStyle
    click node4 goToHeading "Updating DB2 policy record and handling concurrency"
    node4 --> node5["Finalizing policy update and syncing timestamps"]:::HeadingStyle
    click node5 goToHeading "Finalizing policy update and syncing timestamps"
    node3 -->|"No"| node5
    node5 --> node6["Processing VSAM policy update and error handling"]:::HeadingStyle
    click node6 goToHeading "Processing VSAM policy update and error handling"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGUPDB01 (<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>)
- LGUPVS01 (<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)
- LGPOLICY (<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>)
- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  dbcv9("Updating Policy Details (LGUPOL01)") --> kfnb8("Updating Policy details (LGUPDB01)"):::currentEntity
click dbcv9 openCode "base/src/lgupol01.cbl:1"
  
  
click kfnb8 openCode "base/src/lgupdb01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   dbcv9("Updating Policy Details (LGUPOL01)") --> kfnb8("Updating Policy details (LGUPDB01)"):::currentEntity
%% click dbcv9 openCode "<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>:1"
%%   
%%   
%% click kfnb8 openCode "<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
