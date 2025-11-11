---
title: Risk Assessment and Premium Calculation (LGAPDB03) - Overview
---
# Overview

This document describes the flow for orchestrating risk assessment and premium calculation. The process ensures that risk factors for FIRE and CRIME perils are always available, and assigns an application verdict based on the calculated risk score.

## Dependencies

### Program

- LGAPDB03 (<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>)

### Copybook

- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  cnjfd("Enhanced Policy Premium Calculator (LGAPDB01)") --> xwzz9("Risk Assessment and Premium Calculation (LGAPDB03)"):::currentEntity
click cnjfd openCode "base/src/LGAPDB01.cbl:1"
  
  
click xwzz9 openCode "base/src/LGAPDB03.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   cnjfd("Enhanced Policy Premium Calculator (LGAPDB01)") --> xwzz9("Risk Assessment and Premium Calculation (LGAPDB03)"):::currentEntity
%% click cnjfd openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click xwzz9 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
