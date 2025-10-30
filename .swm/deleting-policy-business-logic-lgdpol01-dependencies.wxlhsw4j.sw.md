---
title: Deleting Policy Business Logic (LGDPOL01) - Dependencies
---
# Dependencies

```mermaid
graph TD
  5ywbe("Managing Commercial Policy Operations (LGTESTP4)") --> w3o53("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
click 5ywbe openCode "base/src/lgtestp4.cbl:1"
i1y73("Endowment Policy Menu (LGTESTP2)") --> w3o53("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
click i1y73 openCode "base/src/lgtestp2.cbl:1"
hb5cv("House Policy Menu (LGTESTP3)") --> w3o53("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
click hb5cv openCode "base/src/lgtestp3.cbl:1"
7gzk5("Motor Policy Menu (LGTESTP1)") --> w3o53("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
click 7gzk5 openCode "base/src/lgtestp1.cbl:1"
  w3o53("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity --> pn0xo("LGDPDB01")
click pn0xo openCode "base/src/lgdpdb01.cbl:1"
  pn0xo("LGDPDB01") --> fcgna("LGDPVS01")
click fcgna openCode "base/src/lgdpvs01.cbl:1"
  fcgna("LGDPVS01") --> aufeh("LGSTSQ")
click aufeh openCode "base/src/lgstsq.cbl:1"
  
  
  
pn0xo("LGDPDB01") --> fzgzm("LGSTSQ")
click fzgzm openCode "base/src/lgstsq.cbl:1"
  
  
  
w3o53("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity --> 7ot82("LGSTSQ")
click 7ot82 openCode "base/src/lgstsq.cbl:1"
  
  
  
click w3o53 openCode "base/src/lgdpol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   5ywbe("Managing Commercial Policy Operations (LGTESTP4)") --> w3o53("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
%% click 5ywbe openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%% i1y73("Endowment Policy Menu (LGTESTP2)") --> w3o53("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
%% click i1y73 openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% hb5cv("House Policy Menu (LGTESTP3)") --> w3o53("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
%% click hb5cv openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% 7gzk5("Motor Policy Menu (LGTESTP1)") --> w3o53("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
%% click 7gzk5 openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%%   w3o53("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity --> pn0xo("LGDPDB01")
%% click pn0xo openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%   pn0xo("LGDPDB01") --> fcgna("LGDPVS01")
%% click fcgna openCode "<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>:1"
%%   fcgna("LGDPVS01") --> aufeh("LGSTSQ")
%% click aufeh openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% pn0xo("LGDPDB01") --> fzgzm("LGSTSQ")
%% click fzgzm openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% w3o53("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity --> 7ot82("LGSTSQ")
%% click 7ot82 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% click w3o53 openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>

<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>

<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>

<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
