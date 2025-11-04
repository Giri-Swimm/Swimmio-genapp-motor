---
title: Risk Assessment and Premium Calculation (LGAPDB03)
---
# Overview

This document describes the flow for assessing insurance risk and calculating premiums. The process gathers risk factors for 'FIRE' and 'CRIME', determines a risk verdict, and calculates the premium according to the verdict, using defaults if necessary.

## Dependencies

### Copybook

- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  02kaw("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> jgs9x("Risk Assessment and Premium Calculation (LGAPDB03)"):::currentEntity
click 02kaw openCode "base/src/LGAPDB01.cbl:1"
  
  
click jgs9x openCode "base/src/LGAPDB03.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   02kaw("Calculating Enhanced Insurance Premiums (LGAPDB01)") --> jgs9x("Risk Assessment and Premium Calculation (<SwmToken path="base/src/LGAPDB03.cbl" pos="2:6:6" line-data="       PROGRAM-ID. LGAPDB03.">`LGAPDB03`</SwmToken>)"):::currentEntity
%% click 02kaw openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click jgs9x openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Input and Output Tables/Files used in the Program

| Table / File Name                                                                                                          | Type | Description                                              | Usage Mode | Key Fields / Layout Highlights                                                                                                                                                                                                                                                                               |
| -------------------------------------------------------------------------------------------------------------------------- | ---- | -------------------------------------------------------- | ---------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| <SwmToken path="base/src/LGAPDB03.cbl" pos="51:3:3" line-data="               FROM RISK_FACTORS">`RISK_FACTORS`</SwmToken> | DB2  | Risk factor values by peril type for premium calculation | Input      | <SwmToken path="base/src/LGAPDB03.cbl" pos="50:8:12" line-data="               SELECT FACTOR_VALUE INTO :WS-FIRE-FACTOR">`WS-FIRE-FACTOR`</SwmToken>, <SwmToken path="base/src/LGAPDB03.cbl" pos="62:8:12" line-data="               SELECT FACTOR_VALUE INTO :WS-CRIME-FACTOR">`WS-CRIME-FACTOR`</SwmToken> |

&nbsp;

# Workflow

# Orchestrating the Risk Assessment and Premium Calculation

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Start main insurance logic"] --> node2["Gather risk factors"]
    click node1 openCode "base/src/LGAPDB03.cbl:42:46"
    click node2 openCode "base/src/LGAPDB03.cbl:43:43"
    node2 --> node3{"Calculate verdict"}
    click node3 openCode "base/src/LGAPDB03.cbl:44:44"
    node3 -->|"Approve"| node4["Calculate premiums (approved)"]
    node3 -->|"Reject"| node5["Calculate premiums (rejected)"]
    node3 -->|"Refer"| node6["Calculate premiums (referred)"]
    click node4 openCode "base/src/LGAPDB03.cbl:45:45"
    click node5 openCode "base/src/LGAPDB03.cbl:45:45"
    click node6 openCode "base/src/LGAPDB03.cbl:45:45"
    node4 --> node7["Finish process"]
    node5 --> node7
    node6 --> node7
    click node7 openCode "base/src/LGAPDB03.cbl:46:46"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;

%% Swimm:
%% %%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
%% flowchart TD
%%     node1["Start main insurance logic"] --> node2["Gather risk factors"]
%%     click node1 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:42:46"
%%     click node2 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:43:43"
%%     node2 --> node3{"Calculate verdict"}
%%     click node3 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:44:44"
%%     node3 -->|"Approve"| node4["Calculate premiums (approved)"]
%%     node3 -->|"Reject"| node5["Calculate premiums (rejected)"]
%%     node3 -->|"Refer"| node6["Calculate premiums (referred)"]
%%     click node4 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:45:45"
%%     click node5 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:45:45"
%%     click node6 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:45:45"
%%     node4 --> node7["Finish process"]
%%     node5 --> node7
%%     node6 --> node7
%%     click node7 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:46:46"
%% classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

This section orchestrates the insurance risk assessment and premium calculation process. It ensures that all necessary risk factors are collected before making a decision on the policy and calculating the premium according to the verdict.

| Category        | Rule Name                       | Description                                                                                                                                                                                    |
| --------------- | ------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Data validation | Complete risk factor collection | All risk factors required for assessment must be gathered before any verdict or premium calculation is performed. If a risk factor is missing from the database, a default value must be used. |
| Business logic  | Risk verdict determination      | The risk verdict must be determined based on the collected risk factors, resulting in one of three possible outcomes: Approve, Reject, or Refer.                                               |
| Business logic  | Premium calculation by verdict  | Premium calculation must be performed according to the risk verdict. Approved, rejected, and referred cases each require a distinct premium calculation approach.                              |

<SwmSnippet path="/base/src/LGAPDB03.cbl" line="42">

---

<SwmToken path="base/src/LGAPDB03.cbl" pos="42:1:3" line-data="       MAIN-LOGIC.">`MAIN-LOGIC`</SwmToken> kicks off the process by fetching risk factors, then uses those to determine the verdict and calculate premiums. We call <SwmToken path="base/src/LGAPDB03.cbl" pos="43:3:7" line-data="           PERFORM GET-RISK-FACTORS">`GET-RISK-FACTORS`</SwmToken> first because the rest of the flow depends on having up-to-date risk factor values from the database or defaults if missing.

```cobol
       MAIN-LOGIC.
           PERFORM GET-RISK-FACTORS
           PERFORM CALCULATE-VERDICT
           PERFORM CALCULATE-PREMIUMS
           GOBACK.
```

---

</SwmSnippet>

# Fetching and Defaulting Risk Factors

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Retrieve FIRE risk factor from database"]
    click node1 openCode "base/src/LGAPDB03.cbl:49:53"
    node1 --> node2{"Was FIRE risk factor found?"}
    click node2 openCode "base/src/LGAPDB03.cbl:55:59"
    node2 -->|"Yes"| node3["Use retrieved FIRE risk factor"]
    click node3 openCode "base/src/LGAPDB03.cbl:56:56"
    node2 -->|"No"| node4["Use default FIRE risk factor (0.80)"]
    click node4 openCode "base/src/LGAPDB03.cbl:58:58"
    node3 --> node5["Retrieve CRIME risk factor from database"]
    click node5 openCode "base/src/LGAPDB03.cbl:62:65"
    node4 --> node5
    node5 --> node6{"Was CRIME risk factor found?"}
    click node6 openCode "base/src/LGAPDB03.cbl:67:71"
    node6 -->|"Yes"| node7["Use retrieved CRIME risk factor"]
    click node7 openCode "base/src/LGAPDB03.cbl:68:68"
    node6 -->|"No"| node8["Use default CRIME risk factor (0.60)"]
    click node8 openCode "base/src/LGAPDB03.cbl:70:70"
    node7 --> node9["Provide both risk factors for insurance calculation"]
    click node9 openCode "base/src/LGAPDB03.cbl:48:71"
    node8 --> node9
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;

%% Swimm:
%% %%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
%% flowchart TD
%%     node1["Retrieve FIRE risk factor from database"]
%%     click node1 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:49:53"
%%     node1 --> node2{"Was FIRE risk factor found?"}
%%     click node2 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:55:59"
%%     node2 -->|"Yes"| node3["Use retrieved FIRE risk factor"]
%%     click node3 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:56:56"
%%     node2 -->|"No"| node4["Use default FIRE risk factor (<SwmToken path="base/src/LGAPDB03.cbl" pos="58:3:5" line-data="               MOVE 0.80 TO WS-FIRE-FACTOR">`0.80`</SwmToken>)"]
%%     click node4 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:58:58"
%%     node3 --> node5["Retrieve CRIME risk factor from database"]
%%     click node5 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:62:65"
%%     node4 --> node5
%%     node5 --> node6{"Was CRIME risk factor found?"}
%%     click node6 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:67:71"
%%     node6 -->|"Yes"| node7["Use retrieved CRIME risk factor"]
%%     click node7 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:68:68"
%%     node6 -->|"No"| node8["Use default CRIME risk factor (<SwmToken path="base/src/LGAPDB03.cbl" pos="70:3:5" line-data="               MOVE 0.60 TO WS-CRIME-FACTOR">`0.60`</SwmToken>)"]
%%     click node8 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:70:70"
%%     node7 --> node9["Provide both risk factors for insurance calculation"]
%%     click node9 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:48:71"
%%     node8 --> node9
%% classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

This section ensures that insurance calculations always have valid risk factor values for the 'FIRE' and 'CRIME' peril types, using database values when available and defaulting when necessary.

| Category        | Rule Name                           | Description                                                                                                                                                                                                                                                  |
| --------------- | ----------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Data validation | Peril type restriction              | Only the 'FIRE' and 'CRIME' peril types are considered for risk factor retrieval and defaulting in this section.                                                                                                                                             |
| Business logic  | Use FIRE risk factor from database  | If a risk factor for the 'FIRE' peril type is found in the database, use the retrieved value for insurance calculations.                                                                                                                                     |
| Business logic  | Default FIRE risk factor            | If a risk factor for the 'FIRE' peril type is not found in the database, use the default value of <SwmToken path="base/src/LGAPDB03.cbl" pos="58:3:5" line-data="               MOVE 0.80 TO WS-FIRE-FACTOR">`0.80`</SwmToken> for insurance calculations.   |
| Business logic  | Use CRIME risk factor from database | If a risk factor for the 'CRIME' peril type is found in the database, use the retrieved value for insurance calculations.                                                                                                                                    |
| Business logic  | Default CRIME risk factor           | If a risk factor for the 'CRIME' peril type is not found in the database, use the default value of <SwmToken path="base/src/LGAPDB03.cbl" pos="70:3:5" line-data="               MOVE 0.60 TO WS-CRIME-FACTOR">`0.60`</SwmToken> for insurance calculations. |

<SwmSnippet path="/base/src/LGAPDB03.cbl" line="48">

---

In <SwmToken path="base/src/LGAPDB03.cbl" pos="48:1:5" line-data="       GET-RISK-FACTORS.">`GET-RISK-FACTORS`</SwmToken>, we start by querying the <SwmToken path="base/src/LGAPDB03.cbl" pos="51:3:3" line-data="               FROM RISK_FACTORS">`RISK_FACTORS`</SwmToken> table for the 'FIRE' peril type. The peril types are hardcoded, so this function is only concerned with 'FIRE' and 'CRIME'. If the value isn't found, we'll handle that in the next step.

```cobol
       GET-RISK-FACTORS.
           EXEC SQL
               SELECT FACTOR_VALUE INTO :WS-FIRE-FACTOR
               FROM RISK_FACTORS
               WHERE PERIL_TYPE = 'FIRE'
           END-EXEC.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/LGAPDB03.cbl" line="55">

---

If the database doesn't return a value for 'FIRE', we just assign <SwmToken path="base/src/LGAPDB03.cbl" pos="58:3:5" line-data="               MOVE 0.80 TO WS-FIRE-FACTOR">`0.80`</SwmToken> as the default. This keeps the flow moving even if the data is missing.

```cobol
           IF SQLCODE = 0
               CONTINUE
           ELSE
               MOVE 0.80 TO WS-FIRE-FACTOR
           END-IF.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/LGAPDB03.cbl" line="61">

---

After handling 'FIRE', we do the same thing for 'CRIME'—query the database for its risk factor using another hardcoded string.

```cobol
           EXEC SQL
               SELECT FACTOR_VALUE INTO :WS-CRIME-FACTOR
               FROM RISK_FACTORS
               WHERE PERIL_TYPE = 'CRIME'
           END-EXEC.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/LGAPDB03.cbl" line="67">

---

After both queries, we end up with risk factors for 'FIRE' and 'CRIME'—either from the database or as defaults. These are now ready for the next steps in the flow.

```cobol
           IF SQLCODE = 0
               CONTINUE
           ELSE
               MOVE 0.60 TO WS-CRIME-FACTOR
           END-IF.
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
