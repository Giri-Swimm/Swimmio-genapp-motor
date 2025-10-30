---
title: Calculating Insurance Risk Premiums (LGAPDB03) - Overview
---
# Overview

This document explains how insurance premiums are calculated for a policy. Fire and crime risk factors are sourced from the database or set to defaults, then premiums for each peril are computed and summed, with a discount applied if all perils are present.

## Dependencies

### Copybook

- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  g59o7("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> xw2jj("Calculating Insurance Risk Premiums (LGAPDB03)"):::currentEntity
click g59o7 openCode "base/src/LGAPDB01.cbl:1"
  
  
click xw2jj openCode "base/src/LGAPDB03.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   g59o7("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> xw2jj("Calculating Insurance Risk Premiums (LGAPDB03)"):::currentEntity
%% click g59o7 openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click xw2jj openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
