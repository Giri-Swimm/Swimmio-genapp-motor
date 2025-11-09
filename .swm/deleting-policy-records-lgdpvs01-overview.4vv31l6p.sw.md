---
title: Deleting Policy Records (LGDPVS01) - Overview
---
# Overview

This document describes the flow for deleting a policy record. The process receives a policy deletion request, attempts to remove the policy from the system, and, if unsuccessful, sends detailed error information to audit queues, including additional context if available.

# Technical Overview

```mermaid
sequenceDiagram
  participant LGDPVS01 as LGDPVS01.cbl<br/>(Handles policy deletion and error initiation)
  participant LGSTSQ as LGSTSQ.cbl<br/>(Formats and routes error messages to audit queues)
  LGDPVS01->>LGSTSQ: Send error details and additional context if policy deletion fails
```

## Dependencies

### Program

- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybook

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  bugjh("Deleting Policy Records (LGDPDB01)") --> c61af("Deleting Policy Records (LGDPVS01)"):::currentEntity
click bugjh openCode "base/src/lgdpdb01.cbl:1"
  
  
click c61af openCode "base/src/lgdpvs01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   bugjh("Deleting Policy Records (LGDPDB01)") --> c61af("Deleting Policy Records (LGDPVS01)"):::currentEntity
%% click bugjh openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%   
%%   
%% click c61af openCode "<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
