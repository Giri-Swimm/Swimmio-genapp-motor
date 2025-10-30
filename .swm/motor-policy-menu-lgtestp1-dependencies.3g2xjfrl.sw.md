---
title: Motor Policy Menu (LGTESTP1) - Dependencies
---
# Dependencies

```mermaid
graph TD
  
  myfec("Motor Policy Menu (LGTESTP1)"):::currentEntity --> e33sm("LGIPOL01")
click e33sm openCode "base/src/lgipol01.cbl:1"
  e33sm("LGIPOL01") --> 7nre3("LGIPDB01")
click 7nre3 openCode "base/src/lgipdb01.cbl:1"
  7nre3("LGIPDB01") --> qxx3a("LGSTSQ")
click qxx3a openCode "base/src/lgstsq.cbl:1"
  
  
  
e33sm("LGIPOL01") --> gt207("LGSTSQ")
click gt207 openCode "base/src/lgstsq.cbl:1"
  
  
  
myfec("Motor Policy Menu (LGTESTP1)"):::currentEntity --> weous("LGAPOL01")
click weous openCode "base/src/lgapol01.cbl:1"
  weous("LGAPOL01") --> h2ts8("LGAPDB01")
click h2ts8 openCode "base/src/LGAPDB01.cbl:1"
  h2ts8("LGAPDB01") --> 5vx9g("LGAPDB02")
  
  
h2ts8("LGAPDB01") --> 98q1h("LGAPDB03")
click 98q1h openCode "base/src/LGAPDB03.cbl:1"
  
  
h2ts8("LGAPDB01") --> h5mg5("LGAPDB04")
click h5mg5 openCode "base/src/LGAPDB04.cbl:1"
  
  
  
weous("LGAPOL01") --> z5gk9("LGSTSQ")
click z5gk9 openCode "base/src/lgstsq.cbl:1"
  
  
  
myfec("Motor Policy Menu (LGTESTP1)"):::currentEntity --> m1t30("LGDPOL01")
click m1t30 openCode "base/src/lgdpol01.cbl:1"
  m1t30("LGDPOL01") --> raf2e("LGDPDB01")
click raf2e openCode "base/src/lgdpdb01.cbl:1"
  raf2e("LGDPDB01") --> ht2yf("LGDPVS01")
click ht2yf openCode "base/src/lgdpvs01.cbl:1"
  ht2yf("LGDPVS01") --> wspaf("LGSTSQ")
click wspaf openCode "base/src/lgstsq.cbl:1"
  
  
  
raf2e("LGDPDB01") --> 6qx7q("LGSTSQ")
click 6qx7q openCode "base/src/lgstsq.cbl:1"
  
  
  
m1t30("LGDPOL01") --> mrxuz("LGSTSQ")
click mrxuz openCode "base/src/lgstsq.cbl:1"
  
  
  
myfec("Motor Policy Menu (LGTESTP1)"):::currentEntity --> wl7wr("LGUPOL01")
click wl7wr openCode "base/src/lgupol01.cbl:1"
  wl7wr("LGUPOL01") --> 1s7ts("LGUPDB01")
click 1s7ts openCode "base/src/lgupdb01.cbl:1"
  1s7ts("LGUPDB01") --> krmn2("LGUPVS01")
click krmn2 openCode "base/src/lgupvs01.cbl:1"
  krmn2("LGUPVS01") --> oq6zt("LGSTSQ")
click oq6zt openCode "base/src/lgstsq.cbl:1"
  
  
  
1s7ts("LGUPDB01") --> a2xjs("LGSTSQ")
click a2xjs openCode "base/src/lgstsq.cbl:1"
  
  
  
wl7wr("LGUPOL01") --> y4g92("LGSTSQ")
click y4g92 openCode "base/src/lgstsq.cbl:1"
  
  
  
  
click myfec openCode "base/src/lgtestp1.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   
%%   myfec("Motor Policy Menu (LGTESTP1)"):::currentEntity --> e33sm("LGIPOL01")
%% click e33sm openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%   e33sm("LGIPOL01") --> 7nre3("LGIPDB01")
%% click 7nre3 openCode "<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>:1"
%%   7nre3("LGIPDB01") --> qxx3a("LGSTSQ")
%% click qxx3a openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% e33sm("LGIPOL01") --> gt207("LGSTSQ")
%% click gt207 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% myfec("Motor Policy Menu (LGTESTP1)"):::currentEntity --> weous("LGAPOL01")
%% click weous openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%   weous("LGAPOL01") --> h2ts8("LGAPDB01")
%% click h2ts8 openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   h2ts8("LGAPDB01") --> 5vx9g("LGAPDB02")
%%   
%%   
%% h2ts8("LGAPDB01") --> 98q1h("LGAPDB03")
%% click 98q1h openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:1"
%%   
%%   
%% h2ts8("LGAPDB01") --> h5mg5("LGAPDB04")
%% click h5mg5 openCode "<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% weous("LGAPOL01") --> z5gk9("LGSTSQ")
%% click z5gk9 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% myfec("Motor Policy Menu (LGTESTP1)"):::currentEntity --> m1t30("LGDPOL01")
%% click m1t30 openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%%   m1t30("LGDPOL01") --> raf2e("LGDPDB01")
%% click raf2e openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%%   raf2e("LGDPDB01") --> ht2yf("LGDPVS01")
%% click ht2yf openCode "<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>:1"
%%   ht2yf("LGDPVS01") --> wspaf("LGSTSQ")
%% click wspaf openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% raf2e("LGDPDB01") --> 6qx7q("LGSTSQ")
%% click 6qx7q openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% m1t30("LGDPOL01") --> mrxuz("LGSTSQ")
%% click mrxuz openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% myfec("Motor Policy Menu (LGTESTP1)"):::currentEntity --> wl7wr("LGUPOL01")
%% click wl7wr openCode "<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>:1"
%%   wl7wr("LGUPOL01") --> 1s7ts("LGUPDB01")
%% click 1s7ts openCode "<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>:1"
%%   1s7ts("LGUPDB01") --> krmn2("LGUPVS01")
%% click krmn2 openCode "<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>:1"
%%   krmn2("LGUPVS01") --> oq6zt("LGSTSQ")
%% click oq6zt openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 1s7ts("LGUPDB01") --> a2xjs("LGSTSQ")
%% click a2xjs openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% wl7wr("LGUPOL01") --> y4g92("LGSTSQ")
%% click y4g92 openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%   
%%   
%%   
%%   
%% click myfec openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
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
