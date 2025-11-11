---
title: Writing Insurance Policy Data Records (LGAPVS01) - Overview
---
# Overview

This document explains the flow for preparing and categorizing insurance policy data for storage. The process receives policy and customer information, formats the record according to product type, and writes it to the system file. If the write fails, error details are logged and routed for resolution.

## Dependencies

### Programs

- LGAPVS01 (<SwmPath>[base/src/lgapvs01.cbl](base/src/lgapvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybook

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  ccle5("Inserting Insurance Policy Data (LGAPDB09)") --> 7nhsg("Writing Insurance Policy Data Records (LGAPVS01)"):::currentEntity
click ccle5 openCode "base/src/lgapdb09.cbl:1"
  
  
click 7nhsg openCode "base/src/lgapvs01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   ccle5("Inserting Insurance Policy Data (LGAPDB09)") --> 7nhsg("Writing Insurance Policy Data Records (LGAPVS01)"):::currentEntity
%% click ccle5 openCode "<SwmPath>[base/src/lgapdb09.cbl](base/src/lgapdb09.cbl)</SwmPath>:1"
%%   
%%   
%% click 7nhsg openCode "<SwmPath>[base/src/lgapvs01.cbl](base/src/lgapvs01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
