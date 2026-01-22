# TODO
## Tasks
- [] Erlang
  - [] Tet: User e core
    - [] come gestisco scommesse concorrenti?
    - [] come spawnano i processi?
    - [] Come funziona MnesiaDB?
  - [] Train: Parte admin
- [] Tren: Nginx load balancer
  - [] proxy globale per i client
- [] Train: Frontend
- [] Train: WebSocket
- [] Spring
  - [] Tren: MySQL
  - [] Tren: Maven
  - [] API per login/registrazione
    - [] Token JWT condivisi con Erlang
    - [] Protocollo di sicurezza e validazione JWT

## API endpoints
- API login/registrazione/logout (user e admin)
- JWT validato ma account non in MnesiaDB -> creazione account con 100€ iniziali POST /session {id, timestamp}
- GET /games (tutti oppure uno solo)
- POST /bet {id, token, game_id, amount, choice}
- GET /balance {id, token}
### Admin
- POST /game {admin_token, team1, team2, start_time}
- POST /start_game . result {admin_token, game_id, result}
- POST /stop_betting {admin_token, game_id}
### Keys
- Key for the JWT signing/validation

## Data structures
### MySQL
- User {id, username, password_hash, isAdmin}

### JWT
- payload: {id, isAdmin, expiry time}

### MnesiaDB
isAdmin letto dal JWT

- Account {id, balance}
- Game {id, question_text, opt1_text, opt2_text, result, betting_open, tot_opt1, tot_opt2}
- Bet {bet_id, user_id, game_id, amount, choice}

### Globals
- ip and ports
- commission percentage
- cap parameter
- initial balance amount
- JWT key
