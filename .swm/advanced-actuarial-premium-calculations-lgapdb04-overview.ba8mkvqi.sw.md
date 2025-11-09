---
title: Advanced Actuarial Premium Calculations (LGAPDB04) - Overview
---
# Overview

This document explains how insurance premiums are calculated by evaluating risk factors, applying business rules for experience and schedule modifiers, determining eligible discounts, and producing the final premium and rate factor.

```mermaid
flowchart TD
  node1["Premium Calculation Sequence"]:::HeadingStyle --> node2["Experience Modifier Calculation"]:::HeadingStyle
  click node1 goToHeading "Premium Calculation Sequence"
  node2 --> node3["Schedule Modification Factor"]:::HeadingStyle
  click node2 goToHeading "Experience Modifier Calculation"
  node3 --> node4["Discount Calculation"]:::HeadingStyle
  click node3 goToHeading "Schedule Modification Factor"
  node4 --> node5["Final Premium and Rate Calculation"]:::HeadingStyle
  click node4 goToHeading "Discount Calculation"
  click node5 goToHeading "Final Premium and Rate Calculation"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Copybook

- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  dyq26("Enhanced Policy Premium Calculator (LGAPDB01)") --> 6rbj2("Advanced Actuarial Premium Calculations (LGAPDB04)"):::currentEntity
click dyq26 openCode "base/src/LGAPDB01.cbl:1"
  
  
click 6rbj2 openCode "base/src/LGAPDB04.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   dyq26("Enhanced Policy Premium Calculator (LGAPDB01)") --> 6rbj2("Advanced Actuarial Premium Calculations (LGAPDB04)"):::currentEntity
%% click dyq26 openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click 6rbj2 openCode "<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
