---
title: Inquiring Policy Details (LGIPOL01) - Overview
---
# Overview

This document explains the flow of policy inquiry, which validates requests and retrieves detailed information for endowment, house, motor, or commercial insurance policies. Users receive either the requested policy data or an error code.

# Technical Overview

```mermaid
sequenceDiagram
  participant INQUIRY as LGIPOL01.cbl<br/>(Handles policy inquiry requests)
  participant DB as LGIPDB01.cbl<br/>(Retrieves policy details from the database)
  participant ERROR as LGSTSQ.cbl<br/>(Logs errors and manages error queues)
  INQUIRY->>ERROR: Log error if request is invalid
  INQUIRY->>DB: Request policy details
  DB-->>INQUIRY: Return policy data or error code
  DB->>ERROR: Log error if data retrieval fails
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

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  7t2s4("Motor Policy Menu (LGTESTP1)") --> tun6s("Inquiring Policy Details (LGIPOL01)"):::currentEntity
click 7t2s4 openCode "base/src/lgtestp1.cbl:1"
x1xkx("House Policy Menu (LGTESTP3)") --> tun6s("Inquiring Policy Details (LGIPOL01)"):::currentEntity
click x1xkx openCode "base/src/lgtestp3.cbl:1"
tn6bg("Endowment Policy Menu (LGTESTP2)") --> tun6s("Inquiring Policy Details (LGIPOL01)"):::currentEntity
click tn6bg openCode "base/src/lgtestp2.cbl:1"
7b9zs("Managing Commercial Policies (LGTESTP4)") --> tun6s("Inquiring Policy Details (LGIPOL01)"):::currentEntity
click 7b9zs openCode "base/src/lgtestp4.cbl:1"
  
  
click tun6s openCode "base/src/lgipol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   7t2s4("Motor Policy Menu (LGTESTP1)") --> tun6s("Inquiring Policy Details (LGIPOL01)"):::currentEntity
%% click 7t2s4 openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%% x1xkx("House Policy Menu (LGTESTP3)") --> tun6s("Inquiring Policy Details (LGIPOL01)"):::currentEntity
%% click x1xkx openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% tn6bg("Endowment Policy Menu (LGTESTP2)") --> tun6s("Inquiring Policy Details (LGIPOL01)"):::currentEntity
%% click tn6bg openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% 7b9zs("Managing Commercial Policies (LGTESTP4)") --> tun6s("Inquiring Policy Details (LGIPOL01)"):::currentEntity
%% click 7b9zs openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%%   
%%   
%% click tun6s openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
