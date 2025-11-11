---
title: Updating Policy Records (LGUPVS01) - Overview
---
# Overview

This document explains the process of updating policy records based on the requested policy type. The flow prepares and updates only the relevant fields for the policy type, and logs detailed error events if the update fails. For example, if a motor policy update is requested, only the motor-specific fields are updated and any errors are logged with full context.

## Dependencies

### Programs

- LGUPVS01 (<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybook

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  upupj("Updating Policy details (LGUPDB01)") --> z8u37("Updating Policy Records (LGUPVS01)"):::currentEntity
click upupj openCode "base/src/lgupdb01.cbl:1"
  
  
click z8u37 openCode "base/src/lgupvs01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   upupj("Updating Policy details (LGUPDB01)") --> z8u37("Updating Policy Records (LGUPVS01)"):::currentEntity
%% click upupj openCode "<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>:1"
%%   
%%   
%% click z8u37 openCode "<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
