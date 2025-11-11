---
title: Daily Premium Summary Report Generator (LGAPRPT1) - Overview
---
# Overview

This document describes the flow for generating a daily premium summary report. The process receives premium output records, processes each record to update premium totals, approval status counts, and risk analysis, and outputs a formatted management report for review.

```mermaid
flowchart TD
  node1["Orchestrating the Report Generation Steps"]:::HeadingStyle --> node2["Preparing Date, Time, and Counters"]:::HeadingStyle
  click node1 goToHeading "Orchestrating the Report Generation Steps"
  click node2 goToHeading "Preparing Date, Time, and Counters"
  node2 --> node3["Processing All Input Records"]:::HeadingStyle
  click node3 goToHeading "Processing All Input Records"
  node3 --> node4["Processing a Single Record"]:::HeadingStyle
  click node4 goToHeading "Processing a Single Record"
  node4 --> node5["Generating the Summary Sections"]:::HeadingStyle
  click node5 goToHeading "Generating the Summary Sections"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- LGAPRPT1 (<SwmPath>[base/src/LGAPRPT1.cbl](base/src/LGAPRPT1.cbl)</SwmPath>)

### Copybook

- OUTPUTREC (<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  14hyz("Insurance Policy Premium Calculation Job (LGAPJOB)") --> cednc("Daily Premium Summary Report Generator (LGAPRPT1)"):::currentEntity
click 14hyz openCode "base/cntl/lgapjob.jcl:1"
  
  
click cednc openCode "base/src/LGAPRPT1.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   14hyz("Insurance Policy Premium Calculation Job (LGAPJOB)") --> cednc("Daily Premium Summary Report Generator (LGAPRPT1)"):::currentEntity
%% click 14hyz openCode "<SwmPath>[base/cntl/lgapjob.jcl](base/cntl/lgapjob.jcl)</SwmPath>:1"
%%   
%%   
%% click cednc openCode "<SwmPath>[base/src/LGAPRPT1.cbl](base/src/LGAPRPT1.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
