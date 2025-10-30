---
title: Advanced Actuarial Premium Calculations (LGAPDB04) - Overview
---
# Overview

This document describes the flow for calculating advanced insurance premiums for business properties. Coverage limits, risk scores, business history, and claims data are used to set up exposures and modifiers, calculate base premiums for selected perils, and aggregate all components to produce the final premium and rate factor.

## Dependencies

### Copybook

- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  4k92p("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> qbd6b("Advanced Actuarial Premium Calculations (LGAPDB04)"):::currentEntity
click 4k92p openCode "base/src/LGAPDB01.cbl:1"
  
  
click qbd6b openCode "base/src/LGAPDB04.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   4k92p("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> qbd6b("Advanced Actuarial Premium Calculations (LGAPDB04)"):::currentEntity
%% click 4k92p openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click qbd6b openCode "<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
