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
    %% ===== Livello 1: Load Balancer =====
    LB[Load Balancer / Reverse Proxy] 
    style LB fill:#f9f,stroke:#333,stroke-width:2px

    %% ===== Livello 2: Servizi =====
    subgraph Services Cluster
        direction TB
        WS[Web Server (Stateless)]
        SPAWN[Spawn Service / Main Erlang Process]
        subgraph GAME_CLUSTER[Game Service Cluster (Erlang + Mnesia)]
            NODE1[Game Node 1]
            NODE2[Game Node 2]
        end
    end

    style WS fill:#bbf,stroke:#333
    style SPAWN fill:#bfb,stroke:#333
    style GAME_CLUSTER fill:#dfd,stroke:#333,stroke-dasharray: 5 5
    style NODE1 fill:#dfd,stroke:#333
    style NODE2 fill:#dfd,stroke:#333

    %% ===== Livello 3: Database =====
    SQL[(MySQL Database)]
    style SQL fill:#bbf,stroke:#333

    %% ===== Collegamenti =====
    LB -->|HTTP/REST| WS
    LB -->|Game Traffic| SPAWN
    SPAWN --> NODE1
    SPAWN --> NODE2
    WS --> SQL
```



