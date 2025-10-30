---
title: Updating Policy details (LGUPDB01) - Overview
---
# Overview

This document explains the flow for updating insurance policy records. The process receives a policy update request, validates and transforms the input, applies updates to DB2 and VSAM databases for endowment, house, and motor policies, and ensures accurate business status reporting and error logging.

```mermaid
flowchart TD
  node1["Updating DB2 policy and fetching record
(Updating DB2 policy and fetching record)"]:::HeadingStyle
  click node1 goToHeading "Updating DB2 policy and fetching record"
  node1 --> node2{"Policy type?
(Updating DB2 policy and fetching record)"}:::HeadingStyle
  click node2 goToHeading "Updating DB2 policy and fetching record"
  node2 -->|"Endowment"| node3["Updating endowment policy details"]:::HeadingStyle
  click node3 goToHeading "Updating endowment policy details"
  node2 -->|"House"| node4["Updating house policy details"]:::HeadingStyle
  click node4 goToHeading "Updating house policy details"
  node2 -->|"Motor"| node5["Updating motor policy details"]:::HeadingStyle
  click node5 goToHeading "Updating motor policy details"
  node3 --> node6["Updating VSAM policy records"]:::HeadingStyle
  click node6 goToHeading "Updating VSAM policy records"
  node4 --> node6
  node5 --> node6
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Technical Overview

```mermaid
sequenceDiagram
  participant MAIN as LGUPDB01<br/>*(Policy Update Processor)*
  participant LOGGER as LGSTSQ<br/>*(Error Logger and Queue Manager)*
  participant VSAM as LGUPVS01<br/>*(VSAM Policy Record Updater)*
  MAIN->>LOGGER: Log error and status messages
  MAIN->>VSAM: Update VSAM policy record
  VSAM-->>LOGGER: Log VSAM update errors if needed
```

## Dependencies

### Programs

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
  9kvle("Updating Policy Details (LGUPOL01)") --> 4vafd("Updating Policy details (LGUPDB01)"):::currentEntity
click 9kvle openCode "base/src/lgupol01.cbl:1"
  
  
click 4vafd openCode "base/src/lgupdb01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   9kvle("Updating Policy Details (LGUPOL01)") --> 4vafd("Updating Policy details (LGUPDB01)"):::currentEntity
%% click 9kvle openCode "<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>:1"
%%   
%%   
%% click 4vafd openCode "<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
