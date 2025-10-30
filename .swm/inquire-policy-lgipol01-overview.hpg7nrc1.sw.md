---
title: Inquire Policy (LGIPOL01) - Overview
---
# Overview

This document explains the flow for inquiring about insurance policies. The system validates requests, logs errors with context, and retrieves policy details for supported policy types, returning results or error codes.

# Technical Overview

```mermaid
sequenceDiagram
  participant INQUIRE as LGIPOL01.cbl<br/>(Handles policy inquiry requests)
  participant DB as LGIPDB01.cbl<br/>(Retrieves policy details from the database)
  participant LOG as LGSTSQ.cbl<br/>(Logs error messages and context)
  INQUIRE->>LOG: Log error if input is missing or invalid
  INQUIRE->>DB: Request policy details
  DB->>LOG: Log error if data retrieval fails
  DB-->>INQUIRE: Return policy details or error code
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
  38yc4("Managing Commercial Policy Operations (LGTESTP4)") --> yirbh("Inquire Policy (LGIPOL01)"):::currentEntity
click 38yc4 openCode "base/src/lgtestp4.cbl:1"
3l2xg("Endowment Policy Menu (LGTESTP2)") --> yirbh("Inquire Policy (LGIPOL01)"):::currentEntity
click 3l2xg openCode "base/src/lgtestp2.cbl:1"
wiolz("House Policy Menu (LGTESTP3)") --> yirbh("Inquire Policy (LGIPOL01)"):::currentEntity
click wiolz openCode "base/src/lgtestp3.cbl:1"
o4i62("Motor Policy Menu (LGTESTP1)") --> yirbh("Inquire Policy (LGIPOL01)"):::currentEntity
click o4i62 openCode "base/src/lgtestp1.cbl:1"
  
  
click yirbh openCode "base/src/lgipol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   38yc4("Managing Commercial Policy Operations (LGTESTP4)") --> yirbh("Inquire Policy (LGIPOL01)"):::currentEntity
%% click 38yc4 openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%% 3l2xg("Endowment Policy Menu (LGTESTP2)") --> yirbh("Inquire Policy (LGIPOL01)"):::currentEntity
%% click 3l2xg openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% wiolz("House Policy Menu (LGTESTP3)") --> yirbh("Inquire Policy (LGIPOL01)"):::currentEntity
%% click wiolz openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% o4i62("Motor Policy Menu (LGTESTP1)") --> yirbh("Inquire Policy (LGIPOL01)"):::currentEntity
%% click o4i62 openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%%   
%%   
%% click yirbh openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
