---
title: Deleting Policy Business Logic (LGDPOL01) - Overview
---
# Overview

This document explains the flow for deleting insurance policies. The process validates requests, ensures only supported policy types are deleted, and delegates the deletion to database and file handlers. Outcomes are logged and communicated for traceability.

```mermaid
flowchart TD
    node1["Starting the Policy Deletion Request"]:::HeadingStyle --> node2{"Validating Request and Preparing for Deletion
(Is input data valid and request type supported?)
(Validating Request and Preparing for Deletion)"}:::HeadingStyle
    click node1 goToHeading "Starting the Policy Deletion Request"
    click node2 goToHeading "Validating Request and Preparing for Deletion"
    node2 -->|"No"|node3["Validating Request and Preparing for Deletion
(Validating Request and Preparing for Deletion)"]:::HeadingStyle
    click node3 goToHeading "Validating Request and Preparing for Deletion"
    node2 -->|"Yes"|node4["Executing Database Policy Deletion"]:::HeadingStyle
    click node4 goToHeading "Executing Database Policy Deletion"
    node4 --> node5["Processing and Validating Policy Deletion"]:::HeadingStyle
    click node5 goToHeading "Processing and Validating Policy Deletion"
    node5 --> node6["Deleting Policy from VSAM File"]:::HeadingStyle
    click node6 goToHeading "Deleting Policy from VSAM File"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGDPOL01 (<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>)
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
  q5n2i("Motor Policy Menu (LGTESTP1)") --> e1f1l("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
click q5n2i openCode "base/src/lgtestp1.cbl:1"
sofsu("House Policy Menu (LGTESTP3)") --> e1f1l("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
click sofsu openCode "base/src/lgtestp3.cbl:1"
kzydc("Endowment Policy Menu (LGTESTP2)") --> e1f1l("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
click kzydc openCode "base/src/lgtestp2.cbl:1"
q7il5("Managing Commercial Policies (LGTESTP4)") --> e1f1l("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
click q7il5 openCode "base/src/lgtestp4.cbl:1"
  
  
click e1f1l openCode "base/src/lgdpol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   q5n2i("Motor Policy Menu (LGTESTP1)") --> e1f1l("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
%% click q5n2i openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%% sofsu("House Policy Menu (LGTESTP3)") --> e1f1l("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
%% click sofsu openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% kzydc("Endowment Policy Menu (LGTESTP2)") --> e1f1l("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
%% click kzydc openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% q7il5("Managing Commercial Policies (LGTESTP4)") --> e1f1l("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
%% click q7il5 openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%%   
%%   
%% click e1f1l openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
