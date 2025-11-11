---
title: Advanced Actuarial Premium Calculations (LGAPDB04) - Overview
---
# Overview

This document describes the flow for calculating property insurance premiums. The process determines exposures and insured value, applies experience and schedule modifiers, and finalizes the premium and rate factor based on business rules and regulatory requirements.

## Dependencies

### Program

- LGAPDB04 (<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>)

### Copybook

- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  hal29("Enhanced Policy Premium Calculator (LGAPDB01)") --> o0yt6("Advanced Actuarial Premium Calculations (LGAPDB04)"):::currentEntity
click hal29 openCode "base/src/LGAPDB01.cbl:1"
  
  
click o0yt6 openCode "base/src/LGAPDB04.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   hal29("Enhanced Policy Premium Calculator (LGAPDB01)") --> o0yt6("Advanced Actuarial Premium Calculations (LGAPDB04)"):::currentEntity
%% click hal29 openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click o0yt6 openCode "<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
