---
title: Inquire Policy Details (LGIPDB01) - Overview
---
# Overview

This document describes the flow for retrieving detailed information about insurance policies. The process validates the request, determines the policy type (endowment, house, motor, or commercial), and fetches the corresponding data from the database. The response includes all relevant policy details or an error/status code.

```mermaid
flowchart TD
  node1["Startup and Initial Checks"]:::HeadingStyle
  click node1 goToHeading "Startup and Initial Checks"
  node1 --> node2["Input Mapping and Policy Type Routing"]:::HeadingStyle
  click node2 goToHeading "Input Mapping and Policy Type Routing"
  node2 --> node3["Endowment Policy Data Retrieval"]:::HeadingStyle
  click node3 goToHeading "Endowment Policy Data Retrieval"
  node2 --> node4["House Policy Data Retrieval"]:::HeadingStyle
  click node4 goToHeading "House Policy Data Retrieval"
  node2 --> node5["Motor Policy Data Retrieval"]:::HeadingStyle
  click node5 goToHeading "Motor Policy Data Retrieval"
  node2 --> node6["Other Policy Type Routing"]:::HeadingStyle
  click node6 goToHeading "Other Policy Type Routing"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Technical Overview

```mermaid
sequenceDiagram
  participant MAIN as LGIPDB01.cbl<br/>*(Policy Inquiry and Data Retrieval)*
  participant LOG as LGSTSQ.cbl<br/>*(Centralized Error Logging)*
  MAIN->>MAIN: Validate request and determine policy type
  MAIN->>MAIN: Retrieve policy data (endowment, house, motor, commercial)
  alt Error or abnormal condition
    MAIN->>LOG: Log error and transaction context
  end
  MAIN-->>MAIN: Return policy data or error/status code
```

## Dependencies

### Program

- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- SQLCA
- LGPOLICY (<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>)
- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  mm74z("Inquire Policy (LGIPOL01)") --> cd69w("Inquire Policy Details (LGIPDB01)"):::currentEntity
click mm74z openCode "base/src/lgipol01.cbl:1"
  
  
click cd69w openCode "base/src/lgipdb01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   mm74z("Inquire Policy (LGIPOL01)") --> cd69w("Inquire Policy Details (LGIPDB01)"):::currentEntity
%% click mm74z openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%   
%%   
%% click cd69w openCode "<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
