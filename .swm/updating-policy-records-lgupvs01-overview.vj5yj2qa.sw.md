---
title: Updating Policy Records (LGUPVS01) - Overview
---
# Overview

This document describes the flow for updating policy records. The process receives a policy update request, determines the policy type, prepares the relevant fields for update, and ensures robust error logging for all update operations.

# Technical Overview

```mermaid
sequenceDiagram
  participant UPDATER as LGUPVS01.cbl<br/>*(Policy Record Updater)*
  participant LOGGER as LGSTSQ.cbl<br/>*(Error Message Logger)*
  UPDATER->>LOGGER: Log error message and input data if policy record read or update fails
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
  wobd7("Updating Policy details (LGUPDB01)") --> fkcvu("Updating Policy Records (LGUPVS01)"):::currentEntity
click wobd7 openCode "base/src/lgupdb01.cbl:1"
  
  
click fkcvu openCode "base/src/lgupvs01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   wobd7("Updating Policy details (LGUPDB01)") --> fkcvu("Updating Policy Records (LGUPVS01)"):::currentEntity
%% click wobd7 openCode "<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>:1"
%%   
%%   
%% click fkcvu openCode "<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
