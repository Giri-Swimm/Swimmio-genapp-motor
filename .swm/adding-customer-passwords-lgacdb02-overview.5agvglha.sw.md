---
title: Adding Customer Passwords (LGACDB02) - Overview
---
# Overview

This document describes the flow for adding customer passwords to the secure database table. The process validates requests, logs errors with transaction context, and inserts password records, ensuring traceability and support for troubleshooting.

```mermaid
flowchart TD
    node1["Request Dispatch and Password Handling
(Request Dispatch and Password Handling)"]:::HeadingStyle --> node2{"Is request to add customer password?
(Request Dispatch and Password Handling)"}:::HeadingStyle
    click node1 goToHeading "Request Dispatch and Password Handling"
    node2 -->|"Yes"| node3["Customer Password Insert and Error Handling
(Customer Password Insert and Error Handling)"]:::HeadingStyle
    click node2 goToHeading "Request Dispatch and Password Handling"
    click node3 goToHeading "Customer Password Insert and Error Handling"
    node3 --> node4{"Did database insert succeed?
(Customer Password Insert and Error Handling)"}:::HeadingStyle
    node4 -->|"No"| node5["Log error and return error code
(Customer Password Insert and Error Handling)"]:::HeadingStyle
    click node4 goToHeading "Customer Password Insert and Error Handling"
    click node5 goToHeading "Customer Password Insert and Error Handling"
    node2 -->|"No"| node5
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

# Technical Overview

```mermaid
sequenceDiagram
  participant LGACDB02 as LGACDB02.cbl<br/>*(Handles customer password addition and error logging)*
  participant LGSTSQ as LGSTSQ.cbl<br/>*(Centralized error message queue and logging)*
  LGACDB02->>LGSTSQ: Send error message and transaction context for logging
```

## Dependencies

### Program

- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- LGPOLICY (<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>)
- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  r5xmb("Adding Customer Details (LGACDB01)") --> qqtew("Adding Customer Passwords (LGACDB02)"):::currentEntity
click r5xmb openCode "base/src/lgacdb01.cbl:1"
  
  
click qqtew openCode "base/src/lgacdb02.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   r5xmb("Adding Customer Details (LGACDB01)") --> qqtew("Adding Customer Passwords (LGACDB02)"):::currentEntity
%% click r5xmb openCode "<SwmPath>[base/src/lgacdb01.cbl](base/src/lgacdb01.cbl)</SwmPath>:1"
%%   
%%   
%% click qqtew openCode "<SwmPath>[base/src/lgacdb02.cbl](base/src/lgacdb02.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
