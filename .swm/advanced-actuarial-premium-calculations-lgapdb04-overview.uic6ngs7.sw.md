---
title: Advanced Actuarial Premium Calculations (LGAPDB04) - Overview
---
# Overview

This document outlines the process for calculating an insurance premium by adjusting for claims experience, aggregating premiums for selected perils, and applying taxes and regulatory caps. The calculation ensures the final premium reflects the policyholder's risk profile and complies with business rules.

## Dependencies

### Copybook

- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  se4t9("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> xc324("Advanced Actuarial Premium Calculations (LGAPDB04)"):::currentEntity
click se4t9 openCode "base/src/LGAPDB01.cbl:1"
  
  
click xc324 openCode "base/src/LGAPDB04.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   se4t9("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> xc324("Advanced Actuarial Premium Calculations (LGAPDB04)"):::currentEntity
%% click se4t9 openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click xc324 openCode "<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
