---
title: Updating Policy Records (LGUPVS01) - Dependencies
---
# Dependencies

```mermaid
graph TD
  zuypz("Updating Policy details (LGUPDB01)") --> q0nbc("Updating Policy Records (LGUPVS01)"):::currentEntity
click zuypz openCode "base/src/lgupdb01.cbl:1"
  q0nbc("Updating Policy Records (LGUPVS01)"):::currentEntity --> cvii1("LGSTSQ")
click cvii1 openCode "base/src/lgstsq.cbl:1"
  
  
  
click q0nbc openCode "base/src/lgupvs01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   zuypz("Updating Policy details (LGUPDB01)") --> q0nbc("Updating Policy Records (LGUPVS01)"):::currentEntity
%% click zuypz openCode "<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>:1"
%%   q0nbc("Updating Policy Records (LGUPVS01)"):::currentEntity --> cvii1("LGSTSQ")
%% click cvii1 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% click q0nbc openCode "<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>

<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
