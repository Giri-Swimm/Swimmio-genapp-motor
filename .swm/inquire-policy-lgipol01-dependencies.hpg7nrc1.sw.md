---
title: Inquire Policy (LGIPOL01) - Dependencies
---
# Dependencies

```mermaid
graph TD
  g50d4("Managing Commercial Policy Operations (LGTESTP4)") --> qm814("Inquire Policy (LGIPOL01)"):::currentEntity
click g50d4 openCode "base/src/lgtestp4.cbl:1"
w2xwn("Endowment Policy Menu (LGTESTP2)") --> qm814("Inquire Policy (LGIPOL01)"):::currentEntity
click w2xwn openCode "base/src/lgtestp2.cbl:1"
h9dor("House Policy Menu (LGTESTP3)") --> qm814("Inquire Policy (LGIPOL01)"):::currentEntity
click h9dor openCode "base/src/lgtestp3.cbl:1"
plisg("Motor Policy Menu (LGTESTP1)") --> qm814("Inquire Policy (LGIPOL01)"):::currentEntity
click plisg openCode "base/src/lgtestp1.cbl:1"
  qm814("Inquire Policy (LGIPOL01)"):::currentEntity --> kdlzz("LGIPDB01")
click kdlzz openCode "base/src/lgipdb01.cbl:1"
  kdlzz("LGIPDB01") --> 1y0j0("LGSTSQ")
click 1y0j0 openCode "base/src/lgstsq.cbl:1"
  
  
  
qm814("Inquire Policy (LGIPOL01)"):::currentEntity --> z9ioo("LGSTSQ")
click z9ioo openCode "base/src/lgstsq.cbl:1"
  
  
  
click qm814 openCode "base/src/lgipol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   g50d4("Managing Commercial Policy Operations (LGTESTP4)") --> qm814("Inquire Policy (LGIPOL01)"):::currentEntity
%% click g50d4 openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%% w2xwn("Endowment Policy Menu (LGTESTP2)") --> qm814("Inquire Policy (LGIPOL01)"):::currentEntity
%% click w2xwn openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% h9dor("House Policy Menu (LGTESTP3)") --> qm814("Inquire Policy (LGIPOL01)"):::currentEntity
%% click h9dor openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% plisg("Motor Policy Menu (LGTESTP1)") --> qm814("Inquire Policy (LGIPOL01)"):::currentEntity
%% click plisg openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%%   qm814("Inquire Policy (LGIPOL01)"):::currentEntity --> kdlzz("LGIPDB01")
%% click kdlzz openCode "<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>:1"
%%   kdlzz("LGIPDB01") --> 1y0j0("LGSTSQ")
%% click 1y0j0 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% qm814("Inquire Policy (LGIPOL01)"):::currentEntity --> z9ioo("LGSTSQ")
%% click z9ioo openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% click qm814 openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>

<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>

<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>

<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
