---
title: Risk Assessment and Premium Calculation (LGAPDB03) - Overview
---
# Overview

This document describes the process for calculating insurance premiums for multiple perils. Risk factors are always available for premium calculations, either from the database or as defaults. Premiums are computed for each peril, and a discount is applied if all perils are covered.

## Dependencies

### Copybook

- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  og2a4("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> iimih("Risk Assessment and Premium Calculation (LGAPDB03)"):::currentEntity
click og2a4 openCode "base/src/LGAPDB01.cbl:1"
  
  
click iimih openCode "base/src/LGAPDB03.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   og2a4("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> iimih("Risk Assessment and Premium Calculation (LGAPDB03)"):::currentEntity
%% click og2a4 openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click iimih openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
