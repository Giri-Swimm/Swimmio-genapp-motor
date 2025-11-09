---
title: Updating Policy Details (LGUPOL01) - Overview
---
# Overview

This document describes the flow for updating insurance policy records. The process validates incoming requests, determines the policy type, and updates the relevant database, with comprehensive error logging and audit trails.

```mermaid
flowchart TD
    node1["Starting the Policy Update Flow"]:::HeadingStyle --> node2{"Is input valid?
(Preparing for Policy Update)"}:::HeadingStyle
    node2 -->|"No"|node5["Abort and log error
(Preparing for Policy Update)"]:::HeadingStyle
    node2 -->|"Yes"|node3{"Policy type?
(Preparing for Policy Update)"}:::HeadingStyle
    node3 -->|"DB2"|node4["Updating Policy Records in DB2"]:::HeadingStyle
    node3 -->|"VSAM"|node6["Updating Policy Records in VSAM"]:::HeadingStyle
    node4 --> node7["Return outcome (success or error)
(Preparing for Policy Update)"]:::HeadingStyle
    node6 --> node7
    node5 --> node7

    click node1 goToHeading "Starting the Policy Update Flow"
    click node2 goToHeading "Preparing for Policy Update"
    click node3 goToHeading "Preparing for Policy Update"
    click node4 goToHeading "Updating Policy Records in DB2"
    click node6 goToHeading "Updating Policy Records in VSAM"
    click node5 goToHeading "Preparing for Policy Update"
    click node7 goToHeading "Preparing for Policy Update"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Technical Overview

```mermaid
sequenceDiagram
  participant COORD as LGUPOL01.cbl<br/>*(Policy Update Coordinator)*
  participant LOGGER as LGSTSQ.cbl<br/>*(Audit Logger)*
  participant DB2UPD as LGUPDB01.cbl<br/>*(DB2 Policy Updater)*
  participant VSAMUPD as LGUPVS01.cbl<br/>*(VSAM Policy Updater)*
  COORD->>LOGGER: Log error or audit message
  COORD->>DB2UPD: Update DB2 policy record (if DB2 type)
  COORD->>VSAMUPD: Update VSAM policy record (if VSAM type)
  DB2UPD-->>COORD: Return update outcome
  VSAMUPD-->>COORD: Return update outcome
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

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  9r39u("Motor Policy Menu (LGTESTP1)") --> 0wzdq("Updating Policy Details (LGUPOL01)"):::currentEntity
click 9r39u openCode "base/src/lgtestp1.cbl:1"
3kac2("House Policy Menu (LGTESTP3)") --> 0wzdq("Updating Policy Details (LGUPOL01)"):::currentEntity
click 3kac2 openCode "base/src/lgtestp3.cbl:1"
611f9("Endowment Policy Menu (LGTESTP2)") --> 0wzdq("Updating Policy Details (LGUPOL01)"):::currentEntity
click 611f9 openCode "base/src/lgtestp2.cbl:1"
  
  
click 0wzdq openCode "base/src/lgupol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   9r39u("Motor Policy Menu (LGTESTP1)") --> 0wzdq("Updating Policy Details (LGUPOL01)"):::currentEntity
%% click 9r39u openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%% 3kac2("House Policy Menu (LGTESTP3)") --> 0wzdq("Updating Policy Details (LGUPOL01)"):::currentEntity
%% click 3kac2 openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% 611f9("Endowment Policy Menu (LGTESTP2)") --> 0wzdq("Updating Policy Details (LGUPOL01)"):::currentEntity
%% click 611f9 openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%%   
%%   
%% click 0wzdq openCode "<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
