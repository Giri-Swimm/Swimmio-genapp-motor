---
title: Deleting Policy Records (LGDPDB01) - Dependencies
---
# Dependencies

```mermaid
graph TD
  ce3lq("Deleting Policy Business Logic (LGDPOL01)") --> mw8bi("Deleting Policy Records (LGDPDB01)"):::currentEntity
click ce3lq openCode "base/src/lgdpol01.cbl:1"
  mw8bi("Deleting Policy Records (LGDPDB01)"):::currentEntity --> ym3ub("LGDPVS01")
click ym3ub openCode "base/src/lgdpvs01.cbl:1"
  ym3ub("LGDPVS01") --> eissz("LGSTSQ")
click eissz openCode "base/src/lgstsq.cbl:1"
  
  
  
mw8bi("Deleting Policy Records (LGDPDB01)"):::currentEntity --> 0zmc3("LGSTSQ")
click 0zmc3 openCode "base/src/lgstsq.cbl:1"
  
  
  
click mw8bi openCode "base/src/lgdpdb01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   ce3lq("Deleting Policy Business Logic (LGDPOL01)") --> mw8bi("Deleting Policy Records (LGDPDB01)"):::currentEntity
%% click ce3lq openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%%   mw8bi("Deleting Policy Records (LGDPDB01)"):::currentEntity --> ym3ub("LGDPVS01")
%% click ym3ub openCode "<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>:1"
%%   ym3ub("LGDPVS01") --> eissz("LGSTSQ")
%% click eissz openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% mw8bi("Deleting Policy Records (LGDPDB01)"):::currentEntity --> 0zmc3("LGSTSQ")
%% click 0zmc3 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% click mw8bi openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>

<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>

<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
