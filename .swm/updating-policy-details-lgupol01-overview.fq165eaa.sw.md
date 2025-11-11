---
title: Updating Policy Details (LGUPOL01) - Overview
---
# Overview

This document describes the flow for updating insurance policy details. Incoming requests are validated, the policy type is determined, and updates are coordinated across both the DB2 database and VSAM files. The flow ensures consistent processing and returns status codes to indicate the result.

## Dependencies

### Programs

- LGUPOL01 (<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>)
- LGUPDB01 (<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>)
- LGUPVS01 (<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)
- LGPOLICY (<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>)
- SQLCA

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  j7j2u("Motor Policy Menu (LGTESTP1)") --> ylvak("Updating Policy Details (LGUPOL01)"):::currentEntity
click j7j2u openCode "base/src/lgtestp1.cbl:1"
kwccz("House Policy Menu (LGTESTP3)") --> ylvak("Updating Policy Details (LGUPOL01)"):::currentEntity
click kwccz openCode "base/src/lgtestp3.cbl:1"
nhg37("Endowment Policy Menu (LGTESTP2)") --> ylvak("Updating Policy Details (LGUPOL01)"):::currentEntity
click nhg37 openCode "base/src/lgtestp2.cbl:1"
  
  
click ylvak openCode "base/src/lgupol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   j7j2u("Motor Policy Menu (LGTESTP1)") --> ylvak("Updating Policy Details (LGUPOL01)"):::currentEntity
%% click j7j2u openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%% kwccz("House Policy Menu (LGTESTP3)") --> ylvak("Updating Policy Details (LGUPOL01)"):::currentEntity
%% click kwccz openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% nhg37("Endowment Policy Menu (LGTESTP2)") --> ylvak("Updating Policy Details (LGUPOL01)"):::currentEntity
%% click nhg37 openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%%   
%%   
%% click ylvak openCode "<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
