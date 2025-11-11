---
title: Inquiring Policy Details (LGIPDB01) - Overview
---
# Overview

This document explains how policy details are retrieved for endowment, house, motor, or commercial insurance products. The process receives a customer and policy number, determines the policy type, and returns the relevant policy data or an error code.

```mermaid
flowchart TD
    node1["Startup and Input Validation"]:::HeadingStyle --> node2["Request Routing and Policy Type Dispatch"]:::HeadingStyle
    click node1 goToHeading "Startup and Input Validation"
    click node2 goToHeading "Request Routing and Policy Type Dispatch"
    node2 -->|"Endowment"| node3["Endowment Policy Data Retrieval"]:::HeadingStyle
    click node3 goToHeading "Endowment Policy Data Retrieval"
    node2 -->|"House"| node4["House Policy Data Retrieval"]:::HeadingStyle
    click node4 goToHeading "House Policy Data Retrieval"
    node2 -->|"Motor"| node5["Motor Policy Data Retrieval"]:::HeadingStyle
    click node5 goToHeading "Motor Policy Data Retrieval"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGIPDB01 (<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- SQLCA
- LGPOLICY (<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>)
- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  wos88("Inquiring Policy Details (LGIPOL01)") --> j8y9n("Inquiring Policy Details (LGIPDB01)"):::currentEntity
click wos88 openCode "base/src/lgipol01.cbl:1"
  
  
click j8y9n openCode "base/src/lgipdb01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   wos88("Inquiring Policy Details (LGIPOL01)") --> j8y9n("Inquiring Policy Details (LGIPDB01)"):::currentEntity
%% click wos88 openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%   
%%   
%% click j8y9n openCode "<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
