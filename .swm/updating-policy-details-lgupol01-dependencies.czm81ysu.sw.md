---
title: Updating Policy Details (LGUPOL01) - Dependencies
---
# Dependencies

```mermaid
graph TD
  24msn("Endowment Policy Menu (LGTESTP2)") --> moek2("Updating Policy Details (LGUPOL01)"):::currentEntity
click 24msn openCode "base/src/lgtestp2.cbl:1"
9jckg("House Policy Menu (LGTESTP3)") --> moek2("Updating Policy Details (LGUPOL01)"):::currentEntity
click 9jckg openCode "base/src/lgtestp3.cbl:1"
tj6qy("Motor Policy Menu (LGTESTP1)") --> moek2("Updating Policy Details (LGUPOL01)"):::currentEntity
click tj6qy openCode "base/src/lgtestp1.cbl:1"
  moek2("Updating Policy Details (LGUPOL01)"):::currentEntity --> xer4g("LGUPDB01")
click xer4g openCode "base/src/lgupdb01.cbl:1"
  xer4g("LGUPDB01") --> 7z7ca("LGUPVS01")
click 7z7ca openCode "base/src/lgupvs01.cbl:1"
  7z7ca("LGUPVS01") --> w08vs("LGSTSQ")
click w08vs openCode "base/src/lgstsq.cbl:1"
  
  
  
xer4g("LGUPDB01") --> zzko3("LGSTSQ")
click zzko3 openCode "base/src/lgstsq.cbl:1"
  
  
  
moek2("Updating Policy Details (LGUPOL01)"):::currentEntity --> nvatb("LGSTSQ")
click nvatb openCode "base/src/lgstsq.cbl:1"
  
  
  
click moek2 openCode "base/src/lgupol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   24msn("Endowment Policy Menu (LGTESTP2)") --> moek2("Updating Policy Details (LGUPOL01)"):::currentEntity
%% click 24msn openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% 9jckg("House Policy Menu (LGTESTP3)") --> moek2("Updating Policy Details (LGUPOL01)"):::currentEntity
%% click 9jckg openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% tj6qy("Motor Policy Menu (LGTESTP1)") --> moek2("Updating Policy Details (LGUPOL01)"):::currentEntity
%% click tj6qy openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%%   moek2("Updating Policy Details (LGUPOL01)"):::currentEntity --> xer4g("LGUPDB01")
%% click xer4g openCode "<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>:1"
%%   xer4g("LGUPDB01") --> 7z7ca("LGUPVS01")
%% click 7z7ca openCode "<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>:1"
%%   7z7ca("LGUPVS01") --> w08vs("LGSTSQ")
%% click w08vs openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% xer4g("LGUPDB01") --> zzko3("LGSTSQ")
%% click zzko3 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% moek2("Updating Policy Details (LGUPOL01)"):::currentEntity --> nvatb("LGSTSQ")
%% click nvatb openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% click moek2 openCode "<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>

<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>

<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>

<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>

<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
