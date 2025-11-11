---
title: Logging and Queueing Messages (LGSTSQ) - Overview
---
# Overview

This document describes the flow for routing and preparing messages for logging and further processing. Messages may originate from other programs or terminal users. The flow ensures messages are correctly processed, logged, and queued, supporting consistent communication across business operations.

```mermaid
flowchart TD
    node1["Message Routing and Preparation
(Message Routing and Preparation)"]:::HeadingStyle --> node2{"Source: Program or Terminal?
(Message Routing and Preparation)"}:::HeadingStyle
    click node1 goToHeading "Message Routing and Preparation"
    node2 --> node3{"Message starts with 'Q='?
(Message Routing and Preparation)"}:::HeadingStyle
    node3 -->|"Yes"| node4["Extract extension, process, log, and queue message
(Message Routing and Preparation)"]:::HeadingStyle
    node3 -->|"No"| node5["Process, log, and queue message
(Message Routing and Preparation)"]:::HeadingStyle
    node2 --> node6{"Received from terminal?
(Message Routing and Preparation)"}:::HeadingStyle
    node6 -->|"Yes"| node7["Send acknowledgement to terminal
(Message Routing and Preparation)"]:::HeadingStyle
    node4 --> node7
    node5 --> node7
    click node2 goToHeading "Message Routing and Preparation"
    click node3 goToHeading "Message Routing and Preparation"
    click node4 goToHeading "Message Routing and Preparation"
    click node5 goToHeading "Message Routing and Preparation"
    click node6 goToHeading "Message Routing and Preparation"
    click node7 goToHeading "Message Routing and Preparation"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  gjzc0("Writing Insurance Policy Data Records (LGAPVS01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click gjzc0 openCode "base/src/lgapvs01.cbl:1"
vdt0a("Processing Policy Data (LGAPOL01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click vdt0a openCode "base/src/lgapol01.cbl:1"
l7lgk("Deleting Policy Records (LGDPDB01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click l7lgk openCode "base/src/lgdpdb01.cbl:1"
b1v64("Deleting Policy Records (LGDPVS01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click b1v64 openCode "base/src/lgdpvs01.cbl:1"
v18cu("Adding Customer (LGACUS01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click v18cu openCode "base/src/lgacus01.cbl:1"
1bocg("Adding Customer Passwords (LGACDB02)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click 1bocg openCode "base/src/lgacdb02.cbl:1"
5v5y8("Deleting Policy Business Logic (LGDPOL01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click 5v5y8 openCode "base/src/lgdpol01.cbl:1"
fevqv("Adding Customer Details (LGACDB01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click fevqv openCode "base/src/lgacdb01.cbl:1"
3hnoh("Inquire Customer (LGICUS01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click 3hnoh openCode "base/src/lgicus01.cbl:1"
sj9so("Inquiring Customer Details (LGICDB01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click sj9so openCode "base/src/lgicdb01.cbl:1"
yku39("Inserting Insurance Policy Data (LGAPDB09)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click yku39 openCode "base/src/lgapdb09.cbl:1"
7wod6("Adding Customer Records (LGACVS01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click 7wod6 openCode "base/src/lgacvs01.cbl:1"
7hl3u("Inquiring Policy Details (LGIPDB01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click 7hl3u openCode "base/src/lgipdb01.cbl:1"
x63k5("Updating Policy Records (LGUPVS01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click x63k5 openCode "base/src/lgupvs01.cbl:1"
pqs1i("Updating Policy Details (LGUPOL01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click pqs1i openCode "base/src/lgupol01.cbl:1"
kqogi("Updating Customer Records (LGUCVS01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click kqogi openCode "base/src/lgucvs01.cbl:1"
6yyp3("Updating Policy details (LGUPDB01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click 6yyp3 openCode "base/src/lgupdb01.cbl:1"
htuaa("Updating Customer details (LGUCDB01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click htuaa openCode "base/src/lgucdb01.cbl:1"
x68xr("Updating Customer Details (LGUCUS01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click x68xr openCode "base/src/lgucus01.cbl:1"
bs57h("Inquiring Policy Details (LGIPOL01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
click bs57h openCode "base/src/lgipol01.cbl:1"
  
  
click wpteh openCode "base/src/lgstsq.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   gjzc0("Writing Insurance Policy Data Records (LGAPVS01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click gjzc0 openCode "<SwmPath>[base/src/lgapvs01.cbl](base/src/lgapvs01.cbl)</SwmPath>:1"
%% vdt0a("Processing Policy Data (LGAPOL01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click vdt0a openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%% l7lgk("Deleting Policy Records (LGDPDB01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click l7lgk openCode "<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>:1"
%% b1v64("Deleting Policy Records (LGDPVS01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click b1v64 openCode "<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>:1"
%% v18cu("Adding Customer (LGACUS01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click v18cu openCode "<SwmPath>[base/src/lgacus01.cbl](base/src/lgacus01.cbl)</SwmPath>:1"
%% 1bocg("Adding Customer Passwords (LGACDB02)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click 1bocg openCode "<SwmPath>[base/src/lgacdb02.cbl](base/src/lgacdb02.cbl)</SwmPath>:1"
%% 5v5y8("Deleting Policy Business Logic (LGDPOL01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click 5v5y8 openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%% fevqv("Adding Customer Details (LGACDB01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click fevqv openCode "<SwmPath>[base/src/lgacdb01.cbl](base/src/lgacdb01.cbl)</SwmPath>:1"
%% 3hnoh("Inquire Customer (LGICUS01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click 3hnoh openCode "<SwmPath>[base/src/lgicus01.cbl](base/src/lgicus01.cbl)</SwmPath>:1"
%% sj9so("Inquiring Customer Details (LGICDB01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click sj9so openCode "<SwmPath>[base/src/lgicdb01.cbl](base/src/lgicdb01.cbl)</SwmPath>:1"
%% yku39("Inserting Insurance Policy Data (LGAPDB09)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click yku39 openCode "<SwmPath>[base/src/lgapdb09.cbl](base/src/lgapdb09.cbl)</SwmPath>:1"
%% 7wod6("Adding Customer Records (LGACVS01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click 7wod6 openCode "<SwmPath>[base/src/lgacvs01.cbl](base/src/lgacvs01.cbl)</SwmPath>:1"
%% 7hl3u("Inquiring Policy Details (LGIPDB01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click 7hl3u openCode "<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>:1"
%% x63k5("Updating Policy Records (LGUPVS01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click x63k5 openCode "<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>:1"
%% pqs1i("Updating Policy Details (LGUPOL01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click pqs1i openCode "<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>:1"
%% kqogi("Updating Customer Records (LGUCVS01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click kqogi openCode "<SwmPath>[base/src/lgucvs01.cbl](base/src/lgucvs01.cbl)</SwmPath>:1"
%% 6yyp3("Updating Policy details (LGUPDB01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click 6yyp3 openCode "<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>:1"
%% htuaa("Updating Customer details (LGUCDB01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click htuaa openCode "<SwmPath>[base/src/lgucdb01.cbl](base/src/lgucdb01.cbl)</SwmPath>:1"
%% x68xr("Updating Customer Details (LGUCUS01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click x68xr openCode "<SwmPath>[base/src/lgucus01.cbl](base/src/lgucus01.cbl)</SwmPath>:1"
%% bs57h("Inquiring Policy Details (LGIPOL01)") --> wpteh("Logging and Queueing Messages (LGSTSQ)"):::currentEntity
%% click bs57h openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%   
%%   
%% click wpteh openCode "<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
