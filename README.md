# distributed_betting_system

## Architecture

1. load balancer that acts also as reverse proxy (unified IP from outside) `10.2.1.11`
2. WEB server for serving web application and REST endpoints for authenticatino with JWT `10.2.1.12`
3. MySQL database for user information `10.2.1.27`
4. Main Erlang process for spawning erlang processes `10.2.1.28`
5. Two machines for spawning Erlang game processes `10.2.1.XX/XX`

diagram of the design

```mermaid
graph TD
Client((Client Esterno)) --> LB

subgraph "Entry Point"
LB[Load Balancer / Rev. Proxy]
style LB fill:#f9f,stroke:#333,stroke-width:2px
subgraph "IP: 10.2.1.11"
end
end

LB -->|HTTP/REST| WEB
LB -->|Game Traffic| E_CLUSTER

subgraph "Web Stack"
WEB[Web Server]
SQL[(MySQL Database)]

WEB -.->|Auth JWT + App| Client
WEB -->|Query| SQL

style WEB fill:#bbf,stroke:#333
style SQL fill:#bbf,stroke:#333
end

subgraph "Erlang Cluster"
direction TB
SPAWN[Main Process / Spawn Service]
NODE1[Game Node 1]
NODE2[Game Node 2]

SPAWN -.->|Spawns| NODE1
SPAWN -.->|Spawns| NODE2

style SPAWN fill:#bfb,stroke:#333
style NODE1 fill:#dfd,stroke:#333
style NODE2 fill:#dfd,stroke:#333
end

%% Mapping IPs explicitly in the diagram for clarity
click LB "10.2.1.11"
click WEB "10.2.1.12"
click SQL "10.2.1.27"
click SPAWN "10.2.1.28"
```




