---
title: Daily Premium Summary Report Generator (LGAPRPT1) - Overview
---
# Overview

This document describes the flow for generating a daily premium summary report. Premium output records are processed to produce management insights, including premium totals, risk categorization, and approval status breakdowns.

## Dependencies

### Copybook

- OUTPUTREC (<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  4o73k("Insurance Policy Premium Calculation Job (LGAPJOB)") --> 0xx87("Daily Premium Summary Report Generator (LGAPRPT1)"):::currentEntity
click 4o73k openCode "base/cntl/lgapjob.jcl:1"
  
  
click 0xx87 openCode "base/src/LGAPRPT1.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   4o73k("Insurance Policy Premium Calculation Job (LGAPJOB)") --> 0xx87("Daily Premium Summary Report Generator (LGAPRPT1)"):::currentEntity
%% click 4o73k openCode "<SwmPath>[base/cntl/lgapjob.jcl](base/cntl/lgapjob.jcl)</SwmPath>:1"
%%   
%%   
%% click 0xx87 openCode "<SwmPath>[base/src/LGAPRPT1.cbl](base/src/LGAPRPT1.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
