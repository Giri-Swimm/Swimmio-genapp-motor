---
title: Calculating Enhanced Insurance Premiums (LGAPDB01) - Overview
---
# Overview

This document explains the flow for processing insurance policy applications. Each record is validated, commercial policies receive premium calculations, and non-commercial policies are rejected. The flow outputs processed records with calculated premiums, risk scores, and status, along with summary statistics.

# Technical Overview

```mermaid
sequenceDiagram
  participant MAIN as base/src/LGAPDB01.cbl<br/>*(Main Policy Processor)*
  participant BASIC as base/src/LGAPDB03.cbl<br/>*(Risk and Basic Premium Calculator)*
  participant ACTUARIAL as base/src/LGAPDB04.cbl<br/>*(Enhanced Actuarial Calculator)*
  MAIN->>BASIC: Request risk score & basic premium (for commercial policy)
  BASIC-->>MAIN: Return risk score, verdict, and basic premium
  alt Premium exceeds minimum threshold
    MAIN->>ACTUARIAL: Request enhanced actuarial calculation
    ACTUARIAL-->>MAIN: Return enhanced premium and experience modifier
  end

%% Swimm:
%% sequenceDiagram
%%   participant MAIN as <SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath><br/>*(Main Policy Processor)*
%%   participant BASIC as <SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath><br/>*(Risk and Basic Premium Calculator)*
%%   participant ACTUARIAL as <SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath><br/>*(Enhanced Actuarial Calculator)*
%%   MAIN->>BASIC: Request risk score & basic premium (for commercial policy)
%%   BASIC-->>MAIN: Return risk score, verdict, and basic premium
%%   alt Premium exceeds minimum threshold
%%     MAIN->>ACTUARIAL: Request enhanced actuarial calculation
%%     ACTUARIAL-->>MAIN: Return enhanced premium and experience modifier
%%   end
```

## Dependencies

### Programs

- LGAPDB02
- LGAPDB03 (<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>)
- LGAPDB04 (<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>)

### Copybooks

- SQLCA
- INPUTREC2
- OUTPUTREC
- WORKSTOR
- LGAPACT

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
