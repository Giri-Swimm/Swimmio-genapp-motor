---
title: Deleting Policy Records (LGDPVS01) - Dependencies
---
# Dependencies

```mermaid
graph TD
  lrj8s("Deleting Policy Records (LGDPDB01)") --> 9wopq("Deleting Policy Records (LGDPVS01)"):::currentEntity
click lrj8s openCode "base/src/lgdpdb01.cbl:1"
  9wopq("Deleting Policy Records (LGDPVS01)"):::currentEntity --> b0ldn("LGSTSQ")
click b0ldn openCode "base/src/lgstsq.cbl:1"
  
  
  
click 9wopq openCode "base/src/lgdpvs01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   lrj8s("Deleting Policy Records (LGDPDB01)") --> 9wopq("Deleting Policy Records (LGDPVS01)"):::currentEntity
%% click lrj8s openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%   9wopq("Deleting Policy Records (LGDPVS01)"):::currentEntity --> b0ldn("LGSTSQ")
%% click b0ldn openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% click 9wopq openCode "<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>

<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
