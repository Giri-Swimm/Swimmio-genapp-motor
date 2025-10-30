---
title: Updating Policy Details (LGUPOL01) - Overview
---
# Overview

This document describes the flow for updating insurance policy details for Endowment, House, and Motor policies. Incoming requests are validated and prepared, then updates are delegated to both the DB2 database and VSAM file. The flow ensures data integrity, concurrency control, and comprehensive error logging.

```mermaid
flowchart TD
  node1["Preparing for Policy Update
(Preparing for Policy Update)"]:::HeadingStyle
  click node1 goToHeading "Preparing for Policy Update"
  node1 --> node2{"Is request valid and data sufficient?
(Preparing for Policy Update)"}:::HeadingStyle
  click node2 goToHeading "Preparing for Policy Update"
  node2 -->|"Yes"| node3["Delegating Policy Update to DB2"]:::HeadingStyle
  click node3 goToHeading "Delegating Policy Update to DB2"
  node2 -->|"No"| node6["Abort and log error"]
  node3 --> node4["Validating and Updating Policy Data"]:::HeadingStyle
  click node4 goToHeading "Validating and Updating Policy Data"
  node4 --> node5{"Do timestamps match and update succeed?
(Coordinating DB2 Policy Update)"}:::HeadingStyle
  click node5 goToHeading "Coordinating DB2 Policy Update"
  node5 -->|"Yes"| node7["Updating Policy Record in VSAM"]:::HeadingStyle
  click node7 goToHeading "Updating Policy Record in VSAM"
  node5 -->|"No"| node6
  node6["Abort and log error"]
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Technical Overview

```mermaid
sequenceDiagram
  participant COORD as LGUPOL01.cbl<br/>*(Policy Update Coordinator)*
  participant DB2 as LGUPDB01.cbl<br/>*(DB2 Policy Updater)*
  participant VSAM as LGUPVS01.cbl<br/>*(VSAM Policy File Updater)*
  participant LOG as LGSTSQ.cbl<br/>*(Error Logger)*
  COORD->>DB2: Delegate policy update
  DB2->>LOG: Log errors and update events
  DB2->>VSAM: Trigger VSAM file update
  VSAM->>LOG: Log errors and update events
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
  utuij("Endowment Policy Menu (LGTESTP2)") --> ewpwm("Updating Policy Details (LGUPOL01)"):::currentEntity
click utuij openCode "base/src/lgtestp2.cbl:1"
zaz4h("House Policy Menu (LGTESTP3)") --> ewpwm("Updating Policy Details (LGUPOL01)"):::currentEntity
click zaz4h openCode "base/src/lgtestp3.cbl:1"
5quyo("Motor Policy Menu (LGTESTP1)") --> ewpwm("Updating Policy Details (LGUPOL01)"):::currentEntity
click 5quyo openCode "base/src/lgtestp1.cbl:1"
  
  
click ewpwm openCode "base/src/lgupol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   utuij("Endowment Policy Menu (LGTESTP2)") --> ewpwm("Updating Policy Details (LGUPOL01)"):::currentEntity
%% click utuij openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% zaz4h("House Policy Menu (LGTESTP3)") --> ewpwm("Updating Policy Details (LGUPOL01)"):::currentEntity
%% click zaz4h openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% 5quyo("Motor Policy Menu (LGTESTP1)") --> ewpwm("Updating Policy Details (LGUPOL01)"):::currentEntity
%% click 5quyo openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%%   
%%   
%% click ewpwm openCode "<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
