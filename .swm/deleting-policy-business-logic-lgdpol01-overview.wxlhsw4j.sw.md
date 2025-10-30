---
title: Deleting Policy Business Logic (LGDPOL01) - Overview
---
# Overview

This document explains the flow for deleting insurance policies. The process validates requests, coordinates deletion from DB2 and VSAM databases, and logs errors with full transaction context for audit and monitoring.

```mermaid
flowchart TD
    node1["Validating Input and Preparing for Deletion"]:::HeadingStyle --> node2{"Is request valid and supported?"}
    click node1 goToHeading "Validating Input and Preparing for Deletion"
    node2 -->|"No"| node3["Coordinating Policy Deletion and Error Logging"]:::HeadingStyle
    click node3 goToHeading "Coordinating Policy Deletion and Error Logging"
    node2 -->|"Yes"| node4["Triggering DB2 Policy Deletion
(Triggering DB2 Policy Deletion)"]:::HeadingStyle
    click node4 goToHeading "Triggering DB2 Policy Deletion"
    node4 --> node5{"Did DB2 deletion succeed?
(Triggering DB2 Policy Deletion)"}:::HeadingStyle
    click node5 goToHeading "Triggering DB2 Policy Deletion"
    node5 -->|"No"| node3
    node5 -->|"Yes"| node3

classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Technical Overview

```mermaid
sequenceDiagram
  participant COORD as LGDPOL01.cbl<br/>(Policy Deletion Coordinator)
  participant DB2 as LGDPDB01.cbl<br/>(DB2 Policy Deletion Handler)
  participant VSAM as LGDPVS01.cbl<br/>(VSAM Policy Deletion Handler)
  participant LOGGER as LGSTSQ.cbl<br/>(Centralized Error Logger)
  COORD->>DB2: Pass validated deletion request
  DB2->>VSAM: Trigger VSAM policy deletion
  COORD->>LOGGER: Log errors/audit details
  DB2->>LOGGER: Log errors/audit details
  VSAM->>LOGGER: Log errors/audit details
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

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  s008j("Managing Commercial Policy Operations (LGTESTP4)") --> pesyd("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
click s008j openCode "base/src/lgtestp4.cbl:1"
7qjtw("Endowment Policy Menu (LGTESTP2)") --> pesyd("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
click 7qjtw openCode "base/src/lgtestp2.cbl:1"
bb9c6("House Policy Menu (LGTESTP3)") --> pesyd("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
click bb9c6 openCode "base/src/lgtestp3.cbl:1"
szpwd("Motor Policy Menu (LGTESTP1)") --> pesyd("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
click szpwd openCode "base/src/lgtestp1.cbl:1"
  
  
click pesyd openCode "base/src/lgdpol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   s008j("Managing Commercial Policy Operations (LGTESTP4)") --> pesyd("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
%% click s008j openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%% 7qjtw("Endowment Policy Menu (LGTESTP2)") --> pesyd("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
%% click 7qjtw openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% bb9c6("House Policy Menu (LGTESTP3)") --> pesyd("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
%% click bb9c6 openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% szpwd("Motor Policy Menu (LGTESTP1)") --> pesyd("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
%% click szpwd openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%%   
%%   
%% click pesyd openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
