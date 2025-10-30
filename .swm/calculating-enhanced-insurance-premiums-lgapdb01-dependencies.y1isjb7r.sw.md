---
title: Calculating Enhanced Insurance Premiums (LGAPDB01) - Dependencies
---
# Dependencies

```mermaid
graph TD
  dso43("LGAPJOB") --> ug2pz("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity
click dso43 openCode "base/cntl/lgapjob.jcl:1"
ema0s("Processing and Validating Policy Data (LGAPOL01)") --> ug2pz("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity
click ema0s openCode "base/src/lgapol01.cbl:1"
  ug2pz("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity --> 972y4("LGAPDB02")
  
  
ug2pz("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity --> sygcf("LGAPDB03")
click sygcf openCode "base/src/LGAPDB03.cbl:1"
  
  
ug2pz("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity --> dy123("LGAPDB04")
click dy123 openCode "base/src/LGAPDB04.cbl:1"
  
  
  
click ug2pz openCode "base/src/LGAPDB01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   dso43("LGAPJOB") --> ug2pz("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity
%% click dso43 openCode "<SwmPath>[base/cntl/lgapjob.jcl](base/cntl/lgapjob.jcl)</SwmPath>:1"
%% ema0s("Processing and Validating Policy Data (LGAPOL01)") --> ug2pz("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity
%% click ema0s openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%   ug2pz("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity --> 972y4("LGAPDB02")
%%   
%%   
%% ug2pz("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity --> sygcf("LGAPDB03")
%% click sygcf openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:1"
%%   
%%   
%% ug2pz("Calculating Enhanced Insurance Premiums (LGAPDB01)"):::currentEntity --> dy123("LGAPDB04")
%% click dy123 openCode "<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% click ug2pz openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>

<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>

<SwmPath>[base/src/INPUTREC2.cpy](base/src/INPUTREC2.cpy)</SwmPath>

<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>

<SwmPath>[base/src/WORKSTOR.cpy](base/src/WORKSTOR.cpy)</SwmPath>

<SwmPath>[base/src/LGAPACT.cpy](base/src/LGAPACT.cpy)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
