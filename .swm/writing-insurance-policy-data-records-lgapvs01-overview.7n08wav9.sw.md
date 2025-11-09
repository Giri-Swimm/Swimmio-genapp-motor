---
title: Writing Insurance Policy Data Records (LGAPVS01) - Overview
---
# Overview

This document describes the flow for processing insurance policy requests. The flow receives policy and customer data, determines the request type, and maps the relevant fields to an output record for storage. If the write operation fails, error details are captured and routed to logging systems.

# Technical Overview

```mermaid
sequenceDiagram
  participant MAIN as LGAPVS01.cbl<br/>*(Processes and stores insurance policy requests)*
  participant LOG as LGSTSQ.cbl<br/>*(Captures and routes error details to logging systems)*
  MAIN->>LOG: Route error details for failed write operations
```

## Dependencies

### Program

- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybook

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  abd0p("Inserting Insurance Policy Data (LGAPDB09)") --> o0bru("Writing Insurance Policy Data Records (LGAPVS01)"):::currentEntity
click abd0p openCode "base/src/lgapdb09.cbl:1"
  
  
click o0bru openCode "base/src/lgapvs01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   abd0p("Inserting Insurance Policy Data (LGAPDB09)") --> o0bru("Writing Insurance Policy Data Records (LGAPVS01)"):::currentEntity
%% click abd0p openCode "<SwmPath>[base/src/lgapdb09.cbl](base/src/lgapdb09.cbl)</SwmPath>:1"
%%   
%%   
%% click o0bru openCode "<SwmPath>[base/src/lgapvs01.cbl](base/src/lgapvs01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
