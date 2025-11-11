---
title: Inquiring Policy Details (LGIPOL01) - Overview
---
# Overview

This document explains the flow for inquiring insurance policy details. The process validates requests, logs errors, and retrieves full details for endowment, house, or motor policies based on input identifiers.

## Dependencies

### Programs

- LGIPOL01 (<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>)
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
  s5b6v("Motor Policy Menu (LGTESTP1)") --> 3h45f("Inquiring Policy Details (LGIPOL01)"):::currentEntity
click s5b6v openCode "base/src/lgtestp1.cbl:1"
crk5d("House Policy Menu (LGTESTP3)") --> 3h45f("Inquiring Policy Details (LGIPOL01)"):::currentEntity
click crk5d openCode "base/src/lgtestp3.cbl:1"
0yvr5("Endowment Policy Menu (LGTESTP2)") --> 3h45f("Inquiring Policy Details (LGIPOL01)"):::currentEntity
click 0yvr5 openCode "base/src/lgtestp2.cbl:1"
lrjbt("Managing Commercial Policies (LGTESTP4)") --> 3h45f("Inquiring Policy Details (LGIPOL01)"):::currentEntity
click lrjbt openCode "base/src/lgtestp4.cbl:1"
  
  
click 3h45f openCode "base/src/lgipol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   s5b6v("Motor Policy Menu (LGTESTP1)") --> 3h45f("Inquiring Policy Details (LGIPOL01)"):::currentEntity
%% click s5b6v openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%% crk5d("House Policy Menu (LGTESTP3)") --> 3h45f("Inquiring Policy Details (LGIPOL01)"):::currentEntity
%% click crk5d openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% 0yvr5("Endowment Policy Menu (LGTESTP2)") --> 3h45f("Inquiring Policy Details (LGIPOL01)"):::currentEntity
%% click 0yvr5 openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% lrjbt("Managing Commercial Policies (LGTESTP4)") --> 3h45f("Inquiring Policy Details (LGIPOL01)"):::currentEntity
%% click lrjbt openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%%   
%%   
%% click 3h45f openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
