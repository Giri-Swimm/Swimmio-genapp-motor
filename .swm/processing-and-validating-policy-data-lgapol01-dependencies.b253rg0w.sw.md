---
title: Processing and Validating Policy Data (LGAPOL01) - Dependencies
---
# Dependencies

```mermaid
graph TD
  teyrt("Managing Commercial Policy Operations (LGTESTP4)") --> xgc5f("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity
click teyrt openCode "base/src/lgtestp4.cbl:1"
6f3ux("Endowment Policy Menu (LGTESTP2)") --> xgc5f("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity
click 6f3ux openCode "base/src/lgtestp2.cbl:1"
vndqe("House Policy Menu (LGTESTP3)") --> xgc5f("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity
click vndqe openCode "base/src/lgtestp3.cbl:1"
6gy9h("Motor Policy Menu (LGTESTP1)") --> xgc5f("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity
click 6gy9h openCode "base/src/lgtestp1.cbl:1"
  xgc5f("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity --> pc9j6("LGAPDB01")
click pc9j6 openCode "base/src/LGAPDB01.cbl:1"
  pc9j6("LGAPDB01") --> 7o01w("LGAPDB02")
  
  
pc9j6("LGAPDB01") --> styte("LGAPDB03")
click styte openCode "base/src/LGAPDB03.cbl:1"
  
  
pc9j6("LGAPDB01") --> 64zpx("LGAPDB04")
click 64zpx openCode "base/src/LGAPDB04.cbl:1"
  
  
  
xgc5f("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity --> f1vq7("LGSTSQ")
click f1vq7 openCode "base/src/lgstsq.cbl:1"
  
  
  
click xgc5f openCode "base/src/lgapol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   teyrt("Managing Commercial Policy Operations (LGTESTP4)") --> xgc5f("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity
%% click teyrt openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%% 6f3ux("Endowment Policy Menu (LGTESTP2)") --> xgc5f("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity
%% click 6f3ux openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% vndqe("House Policy Menu (LGTESTP3)") --> xgc5f("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity
%% click vndqe openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%% 6gy9h("Motor Policy Menu (LGTESTP1)") --> xgc5f("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity
%% click 6gy9h openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%%   xgc5f("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity --> pc9j6("LGAPDB01")
%% click pc9j6 openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   pc9j6("LGAPDB01") --> 7o01w("LGAPDB02")
%%   
%%   
%% pc9j6("LGAPDB01") --> styte("LGAPDB03")
%% click styte openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:1"
%%   
%%   
%% pc9j6("LGAPDB01") --> 64zpx("LGAPDB04")
%% click 64zpx openCode "<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% xgc5f("Processing and Validating Policy Data (LGAPOL01)"):::currentEntity --> f1vq7("LGSTSQ")
%% click f1vq7 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% click xgc5f openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>

<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>

<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>

<SwmPath>[base/src/INPUTREC2.cpy](base/src/INPUTREC2.cpy)</SwmPath>

<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>

<SwmPath>[base/src/WORKSTOR.cpy](base/src/WORKSTOR.cpy)</SwmPath>

<SwmPath>[base/src/LGAPACT.cpy](base/src/LGAPACT.cpy)</SwmPath>

<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>

<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
