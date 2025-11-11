---
title: Calculating Commercial Property Risk and Premiums (LGCOMCAL) - Overview
---
# Overview

This document describes the flow for calculating risk scores, determining policy status, and computing premiums for commercial property insurance policies. The process prepares the risk matrix, evaluates risk based on property type and postcode, and applies business rules to determine the policy status and premium.

## Dependencies

### Program

- LGCOMCAL (<SwmPath>[base/src/lgcomcal.cbl](base/src/lgcomcal.cbl)</SwmPath>)

### Copybook

- LGCOMDAT (<SwmPath>[base/src/lgcomdat.cpy](base/src/lgcomdat.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  ynw52("Inserting Insurance Policy Data (LGAPDB09)") --> xfmxb("Calculating Commercial Property Risk and Premiums (LGCOMCAL)"):::currentEntity
click ynw52 openCode "base/src/lgapdb09.cbl:1"
  
  
click xfmxb openCode "base/src/lgcomcal.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   ynw52("Inserting Insurance Policy Data (LGAPDB09)") --> xfmxb("Calculating Commercial Property Risk and Premiums (LGCOMCAL)"):::currentEntity
%% click ynw52 openCode "<SwmPath>[base/src/lgapdb09.cbl](base/src/lgapdb09.cbl)</SwmPath>:1"
%%   
%%   
%% click xfmxb openCode "<SwmPath>[base/src/lgcomcal.cbl](base/src/lgcomcal.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
