---
title: Endowment Policy Menu (LGTESTP2) - Dependencies
---
# Dependencies

```mermaid
graph TD
  
  8p7cz("Endowment Policy Menu (LGTESTP2)"):::currentEntity --> t837l("LGIPOL01")
click t837l openCode "base/src/lgipol01.cbl:1"
  t837l("LGIPOL01") --> f48ta("LGIPDB01")
click f48ta openCode "base/src/lgipdb01.cbl:1"
  f48ta("LGIPDB01") --> e3hlz("LGSTSQ")
click e3hlz openCode "base/src/lgstsq.cbl:1"
  
  
  
t837l("LGIPOL01") --> 2ix7x("LGSTSQ")
click 2ix7x openCode "base/src/lgstsq.cbl:1"
  
  
  
8p7cz("Endowment Policy Menu (LGTESTP2)"):::currentEntity --> lzkdd("LGAPOL01")
click lzkdd openCode "base/src/lgapol01.cbl:1"
  lzkdd("LGAPOL01") --> iy667("LGAPDB01")
click iy667 openCode "base/src/LGAPDB01.cbl:1"
  iy667("LGAPDB01") --> i7qwt("LGAPDB02")
  
  
iy667("LGAPDB01") --> qveu0("LGAPDB03")
click qveu0 openCode "base/src/LGAPDB03.cbl:1"
  
  
iy667("LGAPDB01") --> xvbo7("LGAPDB04")
click xvbo7 openCode "base/src/LGAPDB04.cbl:1"
  
  
  
lzkdd("LGAPOL01") --> m3gth("LGSTSQ")
click m3gth openCode "base/src/lgstsq.cbl:1"
  
  
  
8p7cz("Endowment Policy Menu (LGTESTP2)"):::currentEntity --> 2j7cq("LGDPOL01")
click 2j7cq openCode "base/src/lgdpol01.cbl:1"
  2j7cq("LGDPOL01") --> ia9xs("LGDPDB01")
click ia9xs openCode "base/src/lgdpdb01.cbl:1"
  ia9xs("LGDPDB01") --> 4rjf1("LGDPVS01")
click 4rjf1 openCode "base/src/lgdpvs01.cbl:1"
  4rjf1("LGDPVS01") --> 5ess8("LGSTSQ")
click 5ess8 openCode "base/src/lgstsq.cbl:1"
  
  
  
ia9xs("LGDPDB01") --> 73eum("LGSTSQ")
click 73eum openCode "base/src/lgstsq.cbl:1"
  
  
  
2j7cq("LGDPOL01") --> faegl("LGSTSQ")
click faegl openCode "base/src/lgstsq.cbl:1"
  
  
  
8p7cz("Endowment Policy Menu (LGTESTP2)"):::currentEntity --> ur9cx("LGUPOL01")
click ur9cx openCode "base/src/lgupol01.cbl:1"
  ur9cx("LGUPOL01") --> 2la7j("LGUPDB01")
click 2la7j openCode "base/src/lgupdb01.cbl:1"
  2la7j("LGUPDB01") --> huor5("LGUPVS01")
click huor5 openCode "base/src/lgupvs01.cbl:1"
  huor5("LGUPVS01") --> 3lkhn("LGSTSQ")
click 3lkhn openCode "base/src/lgstsq.cbl:1"
  
  
  
2la7j("LGUPDB01") --> pes7d("LGSTSQ")
click pes7d openCode "base/src/lgstsq.cbl:1"
  
  
  
ur9cx("LGUPOL01") --> yvz7u("LGSTSQ")
click yvz7u openCode "base/src/lgstsq.cbl:1"
  
  
  
  
click 8p7cz openCode "base/src/lgtestp2.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   
%%   8p7cz("Endowment Policy Menu (LGTESTP2)"):::currentEntity --> t837l("LGIPOL01")
%% click t837l openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%   t837l("LGIPOL01") --> f48ta("LGIPDB01")
%% click f48ta openCode "<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>:1"
%%   f48ta("LGIPDB01") --> e3hlz("LGSTSQ")
%% click e3hlz openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% t837l("LGIPOL01") --> 2ix7x("LGSTSQ")
%% click 2ix7x openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 8p7cz("Endowment Policy Menu (LGTESTP2)"):::currentEntity --> lzkdd("LGAPOL01")
%% click lzkdd openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%   lzkdd("LGAPOL01") --> iy667("LGAPDB01")
%% click iy667 openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   iy667("LGAPDB01") --> i7qwt("LGAPDB02")
%%   
%%   
%% iy667("LGAPDB01") --> qveu0("LGAPDB03")
%% click qveu0 openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:1"
%%   
%%   
%% iy667("LGAPDB01") --> xvbo7("LGAPDB04")
%% click xvbo7 openCode "<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% lzkdd("LGAPOL01") --> m3gth("LGSTSQ")
%% click m3gth openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 8p7cz("Endowment Policy Menu (LGTESTP2)"):::currentEntity --> 2j7cq("LGDPOL01")
%% click 2j7cq openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%%   2j7cq("LGDPOL01") --> ia9xs("LGDPDB01")
%% click ia9xs openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%   ia9xs("LGDPDB01") --> 4rjf1("LGDPVS01")
%% click 4rjf1 openCode "<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>:1"
%%   4rjf1("LGDPVS01") --> 5ess8("LGSTSQ")
%% click 5ess8 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% ia9xs("LGDPDB01") --> 73eum("LGSTSQ")
%% click 73eum openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 2j7cq("LGDPOL01") --> faegl("LGSTSQ")
%% click faegl openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 8p7cz("Endowment Policy Menu (LGTESTP2)"):::currentEntity --> ur9cx("LGUPOL01")
%% click ur9cx openCode "<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>:1"
%%   ur9cx("LGUPOL01") --> 2la7j("LGUPDB01")
%% click 2la7j openCode "<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>:1"
%%   2la7j("LGUPDB01") --> huor5("LGUPVS01")
%% click huor5 openCode "<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>:1"
%%   huor5("LGUPVS01") --> 3lkhn("LGSTSQ")
%% click 3lkhn openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 2la7j("LGUPDB01") --> pes7d("LGSTSQ")
%% click pes7d openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% ur9cx("LGUPOL01") --> yvz7u("LGSTSQ")
%% click yvz7u openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%%   
%% click 8p7cz openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>

<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>

<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>

<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>

<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>

<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>

<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>

<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>

<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>

<SwmPath>[base/src/INPUTREC2.cpy](base/src/INPUTREC2.cpy)</SwmPath>

<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>

<SwmPath>[base/src/WORKSTOR.cpy](base/src/WORKSTOR.cpy)</SwmPath>

<SwmPath>[base/src/LGAPACT.cpy](base/src/LGAPACT.cpy)</SwmPath>

<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>

<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>

<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>

<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>

<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>

<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
