---
title: Risk Assessment and Premium Calculation (LGAPDB03) - Overview
---
# Overview

This document explains the flow for evaluating insurance risk and calculating premiums. The process ensures the use of current risk factors, determines the application verdict based on risk score, and calculates the total premium, including discounts for full peril coverage.

## Dependencies

### Copybook

- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  4twaz("Enhanced Policy Premium Calculator (LGAPDB01)") --> etoo4("Risk Assessment and Premium Calculation (LGAPDB03)"):::currentEntity
click 4twaz openCode "base/src/LGAPDB01.cbl:1"
  
  
click etoo4 openCode "base/src/LGAPDB03.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   4twaz("Enhanced Policy Premium Calculator (LGAPDB01)") --> etoo4("Risk Assessment and Premium Calculation (LGAPDB03)"):::currentEntity
%% click 4twaz openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click etoo4 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
