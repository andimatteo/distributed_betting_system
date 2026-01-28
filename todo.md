# TODO
## Tasks
- [x] Erlang
  - [x] Tet: User e core
    - [x] come gestisco scommesse concorrenti?
    - [x] come spawnano i processi?
    - [x] Come funziona MnesiaDB?
  - [x] Tet: Parte admin
- [x] Tren: Nginx load balancer
  - [x] proxy globale per i client
- [x] Train: Frontend
- [x] Train: WebSocket
- [x] Spring
  - [x] Tren: MySQL
  - [x] Tren: Maven
  - [x] API per login/registrazione
    - [x] Token JWT condivisi con Erlang (to see further with erlang implementation)
    - [x] Protocollo di sicurezza e validazione JWT
- Generale
  - [X] Utenti non registrati possono vedere cose
  - [] Separare vecchie scommesse da quelle attive
  - [X] Data di bet da correggere (1970)
  - [X] Togliere alert e sostituire con modal
  - [X] Conto admin da mostrare
  - [] Aggiornamento volume totale per ogni scommessa
  - [X] Togliere numero participants
  - [] Togliere console log generali
  - [] Togliere nel websocket message di vincita il totale pagato e i vincitori
  - [] Aggiornamento chiusura scommesse sulla my bets, bilancio in bybets e mybets in detailed view
  - [] Refreshare solo quello che va refreshato quando arriva il websocket message
  - [X] Aggiungere POST /balance per aumentare il saldo
  - [X] Display bet amount input only after selecting an option
  - [] Redirect to login if JWT expired
  - [X] Disabilitare meglio le opzioni delle scommesse chiuse
  - [X] CSS pulsanti filtri in mybets

OPZIONALE 
  - [] separare websocket tra bet page e dashboard
  - [] id fatti bene e non il counter condiviso

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
