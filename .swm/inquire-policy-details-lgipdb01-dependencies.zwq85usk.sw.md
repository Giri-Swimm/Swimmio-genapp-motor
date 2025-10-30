---
title: Inquire Policy Details (LGIPDB01) - Dependencies
---
# Dependencies

```mermaid
graph TD
  i25iz("Inquire Policy (LGIPOL01)") --> 6n8qv("Inquire Policy Details (LGIPDB01)"):::currentEntity
click i25iz openCode "base/src/lgipol01.cbl:1"
  6n8qv("Inquire Policy Details (LGIPDB01)"):::currentEntity --> 7ud0z("LGSTSQ")
click 7ud0z openCode "base/src/lgstsq.cbl:1"
  
  
  
click 6n8qv openCode "base/src/lgipdb01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   i25iz("Inquire Policy (LGIPOL01)") --> 6n8qv("Inquire Policy Details (LGIPDB01)"):::currentEntity
%% click i25iz openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%   6n8qv("Inquire Policy Details (LGIPDB01)"):::currentEntity --> 7ud0z("LGSTSQ")
%% click 7ud0z openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% click 6n8qv openCode "<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>

<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>

<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
