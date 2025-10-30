---
title: Updating Policy details (LGUPDB01) - Dependencies
---
# Dependencies

```mermaid
graph TD
  oc9ro("Updating Policy Details (LGUPOL01)") --> rlyb5("Updating Policy details (LGUPDB01)"):::currentEntity
click oc9ro openCode "base/src/lgupol01.cbl:1"
  rlyb5("Updating Policy details (LGUPDB01)"):::currentEntity --> jpvmi("LGUPVS01")
click jpvmi openCode "base/src/lgupvs01.cbl:1"
  jpvmi("LGUPVS01") --> xmynb("LGSTSQ")
click xmynb openCode "base/src/lgstsq.cbl:1"
  
  
  
rlyb5("Updating Policy details (LGUPDB01)"):::currentEntity --> jzarb("LGSTSQ")
click jzarb openCode "base/src/lgstsq.cbl:1"
  
  
  
click rlyb5 openCode "base/src/lgupdb01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   oc9ro("Updating Policy Details (LGUPOL01)") --> rlyb5("Updating Policy details (LGUPDB01)"):::currentEntity
%% click oc9ro openCode "<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>:1"
%%   rlyb5("Updating Policy details (LGUPDB01)"):::currentEntity --> jpvmi("LGUPVS01")
%% click jpvmi openCode "<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>:1"
%%   jpvmi("LGUPVS01") --> xmynb("LGSTSQ")
%% click xmynb openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% rlyb5("Updating Policy details (LGUPDB01)"):::currentEntity --> jzarb("LGSTSQ")
%% click jzarb openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% click rlyb5 openCode "<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>

<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>

<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>

<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
