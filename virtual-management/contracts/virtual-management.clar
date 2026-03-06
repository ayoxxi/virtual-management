;; Virtual Management - Cross-Chain Guild DAO Platform

;; This contract implements the core Virtual Management ecosystem:
;;   - Guild creation and membership management
;;   - Dual-token governance (governance + reputation)
;;   - Multi-sig treasury with time-locked proposals
;;   - Reputation NFT (SIP-009 compatible)
;;   - Performance-based reward distribution
;;   - Play-to-earn + dividend earning streams

;; ============================================================
;; TRAITS
;; ============================================================

(define-trait sip-009-nft-trait
  (
    (get-last-token-id () (response uint uint))
    (get-token-uri (uint) (response (optional (string-ascii 256)) uint))
    (get-owner (uint) (response (optional principal) uint))
    (transfer (uint principal principal) (response bool uint))
  )
)

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED        (err u100))
(define-constant ERR-GUILD-NOT-FOUND       (err u101))
(define-constant ERR-ALREADY-MEMBER        (err u102))
(define-constant ERR-NOT-MEMBER            (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS    (err u104))
(define-constant ERR-PROPOSAL-NOT-FOUND    (err u105))
(define-constant ERR-ALREADY-VOTED         (err u106))
(define-constant ERR-PROPOSAL-EXPIRED      (err u107))
(define-constant ERR-PROPOSAL-ACTIVE       (err u108))
(define-constant ERR-TIMELOCK-PENDING      (err u109))
(define-constant ERR-INVALID-AMOUNT        (err u110))
(define-constant ERR-TOKEN-NOT-FOUND       (err u111))
(define-constant ERR-TRANSFER-FAILED       (err u112))
(define-constant ERR-QUORUM-NOT-MET        (err u113))
(define-constant ERR-ALREADY-EXECUTED      (err u114))
(define-constant ERR-GUILD-LIMIT           (err u115))

;; Governance parameters
(define-constant PROPOSAL-DURATION         u1440)   ;; ~10 days in blocks
(define-constant TIMELOCK-BLOCKS           u144)    ;; ~1 day in blocks
(define-constant QUORUM-THRESHOLD          u51)     ;; 51% quorum required
(define-constant MAX-GUILD-MEMBERS         u100)
(define-constant REPUTATION-MINT-RATE      u10)     ;; rep tokens per reward unit
(define-constant GOVERNANCE-MINT-AMOUNT    u1000)   ;; gov tokens on guild join
(define-constant DIVIDEND-INTERVAL         u144)    ;; blocks between dividends

;; ============================================================
;; DATA VARS
;; ============================================================

(define-data-var guild-nonce         uint u0)
(define-data-var proposal-nonce      uint u0)
(define-data-var rep-token-nonce     uint u0)
(define-data-var total-gov-supply    uint u0)
(define-data-var total-rep-supply    uint u0)
(define-data-var platform-fee-bps    uint u250)  ;; 2.5% platform fee

;; ============================================================
;; FUNGIBLE TOKENS
;; ============================================================

;; Governance token - for voting weight
(define-fungible-token governance-token)

;; Reputation token - earned via performance
(define-fungible-token reputation-token)

;; ============================================================
;; NON-FUNGIBLE TOKEN (Reputation NFT - SIP-009)
;; ============================================================

(define-non-fungible-token reputation-nft uint)

;; ============================================================
;; DATA MAPS
;; ============================================================

;; Guild registry
(define-map guilds
  { guild-id: uint }
  {
    name:            (string-ascii 64),
    description:     (string-ascii 256),
    founder:         principal,
    treasury:        uint,           ;; STX balance in micro-STX
    member-count:    uint,
    created-at:      uint,
    active:          bool,
    last-dividend-block: uint
  }
)

;; Guild membership
(define-map guild-members
  { guild-id: uint, member: principal }
  {
    role:            (string-ascii 16),  ;; "founder" | "officer" | "member"
    joined-at:       uint,
    contributions:   uint,              ;; cumulative contribution score
    gov-staked:      uint               ;; governance tokens staked in guild
  }
)

;; Governance proposals (multi-sig treasury proposals)
(define-map proposals
  { proposal-id: uint }
  {
    guild-id:        uint,
    proposer:        principal,
    title:           (string-ascii 128),
    description:     (string-ascii 512),
    amount:          uint,             ;; STX amount to transfer
    recipient:       principal,
    votes-for:       uint,
    votes-against:   uint,
    created-at:      uint,
    execute-after:   uint,             ;; timelock block height
    executed:        bool,
    cancelled:       bool
  }
)

;; Vote records (one vote per member per proposal)
(define-map votes
  { proposal-id: uint, voter: principal }
  { support: bool, weight: uint }
)

;; Reputation NFT metadata
(define-map rep-nft-data
  { token-id: uint }
  {
    owner:           principal,
    guild-id:        uint,
    score:           uint,
    category:        (string-ascii 32),   ;; "leadership" | "trading" | "combat"
    issued-at:       uint,
    uri:             (string-ascii 256)
  }
)

;; Player reputation scores (aggregated across guilds)
(define-map player-reputation
  { player: principal }
  {
    total-score:     uint,
    guilds-led:      uint,
    proposals-made:  uint,
    rewards-claimed: uint
  }
)

;; Consulting fees - reputation-based income
(define-map consulting-fees
  { consultant: principal, client: principal }
  {
    fee-per-session: uint,
    sessions:        uint,
    active:          bool
  }
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

(define-private (is-guild-member (guild-id uint) (who principal))
  (is-some (map-get? guild-members { guild-id: guild-id, member: who }))
)

(define-private (get-member-role (guild-id uint) (who principal))
  (match (map-get? guild-members { guild-id: guild-id, member: who })
    m (get role m)
    "none"
  )
)

(define-private (is-guild-officer (guild-id uint) (who principal))
  (let ((role (get-member-role guild-id who)))
    (or (is-eq role "founder") (is-eq role "officer"))
  )
)

(define-private (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-bps)) u10000)
)

(define-private (update-player-reputation (player principal) (score-delta uint))
  (let (
    (current (default-to
      { total-score: u0, guilds-led: u0, proposals-made: u0, rewards-claimed: u0 }
      (map-get? player-reputation { player: player })
    ))
  )
    (map-set player-reputation
      { player: player }
      (merge current { total-score: (+ (get total-score current) score-delta) })
    )
  )
)

;; ============================================================
;; GUILD MANAGEMENT
;; ============================================================

;; Create a new guild DAO
(define-public (create-guild
  (name (string-ascii 64))
  (description (string-ascii 256))
  (initial-treasury uint)
)
  (let (
    (guild-id (+ (var-get guild-nonce) u1))
    (fee (calculate-platform-fee initial-treasury))
    (net-treasury (- initial-treasury fee))
  )
    (asserts! (> (len name) u0) ERR-INVALID-AMOUNT)
    (asserts! (>= initial-treasury fee) ERR-INSUFFICIENT-FUNDS)

    ;; Transfer initial treasury + fee from founder
    (try! (stx-transfer? initial-treasury tx-sender (as-contract tx-sender)))

    ;; Register guild
    (map-set guilds { guild-id: guild-id }
      {
        name:                name,
        description:         description,
        founder:             tx-sender,
        treasury:            net-treasury,
        member-count:        u1,
        created-at:          block-height,
        active:              true,
        last-dividend-block: block-height
      }
    )

    ;; Register founder as member
    (map-set guild-members { guild-id: guild-id, member: tx-sender }
      {
        role:          "founder",
        joined-at:     block-height,
        contributions: u0,
        gov-staked:    GOVERNANCE-MINT-AMOUNT
      }
    )

    ;; Mint governance tokens to founder
    (try! (ft-mint? governance-token GOVERNANCE-MINT-AMOUNT tx-sender))
    (var-set total-gov-supply (+ (var-get total-gov-supply) GOVERNANCE-MINT-AMOUNT))

    ;; Update reputation - guild founder
    (update-player-reputation tx-sender u50)

    ;; Increment nonce
    (var-set guild-nonce guild-id)

    (ok guild-id)
  )
)

;; Join an existing guild
(define-public (join-guild (guild-id uint))
  (let (
    (guild (unwrap! (map-get? guilds { guild-id: guild-id }) ERR-GUILD-NOT-FOUND))
  )
    (asserts! (get active guild) ERR-GUILD-NOT-FOUND)
    (asserts! (not (is-guild-member guild-id tx-sender)) ERR-ALREADY-MEMBER)
    (asserts! (< (get member-count guild) MAX-GUILD-MEMBERS) ERR-GUILD-LIMIT)

    ;; Register as member
    (map-set guild-members { guild-id: guild-id, member: tx-sender }
      {
        role:          "member",
        joined-at:     block-height,
        contributions: u0,
        gov-staked:    GOVERNANCE-MINT-AMOUNT
      }
    )

    ;; Update member count
    (map-set guilds { guild-id: guild-id }
      (merge guild { member-count: (+ (get member-count guild) u1) })
    )

    ;; Mint governance tokens to new member
    (try! (ft-mint? governance-token GOVERNANCE-MINT-AMOUNT tx-sender))
    (var-set total-gov-supply (+ (var-get total-gov-supply) GOVERNANCE-MINT-AMOUNT))

    (ok true)
  )
)

;; Promote a member to officer (founder only)
(define-public (promote-member (guild-id uint) (member principal))
  (let (
    (guild (unwrap! (map-get? guilds { guild-id: guild-id }) ERR-GUILD-NOT-FOUND))
    (membership (unwrap! (map-get? guild-members { guild-id: guild-id, member: member }) ERR-NOT-MEMBER))
  )
    (asserts! (is-eq (get founder guild) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-guild-member guild-id member) ERR-NOT-MEMBER)

    (map-set guild-members { guild-id: guild-id, member: member }
      (merge membership { role: "officer" })
    )
    (ok true)
  )
)

;; Deposit STX into guild treasury
(define-public (deposit-treasury (guild-id uint) (amount uint))
  (let (
    (guild (unwrap! (map-get? guilds { guild-id: guild-id }) ERR-GUILD-NOT-FOUND))
  )
    (asserts! (is-guild-member guild-id tx-sender) ERR-NOT-MEMBER)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)

    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    (map-set guilds { guild-id: guild-id }
      (merge guild { treasury: (+ (get treasury guild) amount) })
    )

    ;; Record contribution
    (match (map-get? guild-members { guild-id: guild-id, member: tx-sender })
      m (map-set guild-members { guild-id: guild-id, member: tx-sender }
          (merge m { contributions: (+ (get contributions m) amount) }))
      false
    )

    (update-player-reputation tx-sender (/ amount u1000000))
    (ok true)
  )
)

;; ============================================================
;; GOVERNANCE - PROPOSALS & VOTING
;; ============================================================

;; Create a treasury spend proposal (multi-sig)
(define-public (create-proposal
  (guild-id uint)
  (title (string-ascii 128))
  (description (string-ascii 512))
  (amount uint)
  (recipient principal)
)
  (let (
    (guild (unwrap! (map-get? guilds { guild-id: guild-id }) ERR-GUILD-NOT-FOUND))
    (proposal-id (+ (var-get proposal-nonce) u1))
  )
    (asserts! (is-guild-officer guild-id tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (<= amount (get treasury guild)) ERR-INSUFFICIENT-FUNDS)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)

    (map-set proposals { proposal-id: proposal-id }
      {
        guild-id:      guild-id,
        proposer:      tx-sender,
        title:         title,
        description:   description,
        amount:        amount,
        recipient:     recipient,
        votes-for:     u0,
        votes-against: u0,
        created-at:    block-height,
        execute-after: (+ block-height PROPOSAL-DURATION TIMELOCK-BLOCKS),
        executed:      false,
        cancelled:     false
      }
    )

    ;; Update proposer reputation
    (match (map-get? player-reputation { player: tx-sender })
      r (map-set player-reputation { player: tx-sender }
          (merge r { proposals-made: (+ (get proposals-made r) u1) }))
      false
    )

    (var-set proposal-nonce proposal-id)
    (ok proposal-id)
  )
)

;; Vote on a proposal using governance token weight
(define-public (vote-on-proposal (proposal-id uint) (support bool))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (guild-id (get guild-id proposal))
    (vote-weight (ft-get-balance governance-token tx-sender))
  )
    (asserts! (is-guild-member guild-id tx-sender) ERR-NOT-MEMBER)
    (asserts! (< block-height (+ (get created-at proposal) PROPOSAL-DURATION)) ERR-PROPOSAL-EXPIRED)
    (asserts! (not (get executed proposal)) ERR-ALREADY-EXECUTED)
    (asserts! (not (get cancelled proposal)) ERR-PROPOSAL-EXPIRED)
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    (asserts! (> vote-weight u0) ERR-INSUFFICIENT-FUNDS)

    ;; Record vote
    (map-set votes { proposal-id: proposal-id, voter: tx-sender }
      { support: support, weight: vote-weight }
    )

    ;; Tally
    (map-set proposals { proposal-id: proposal-id }
      (if support
        (merge proposal { votes-for: (+ (get votes-for proposal) vote-weight) })
        (merge proposal { votes-against: (+ (get votes-against proposal) vote-weight) })
      )
    )

    (ok true)
  )
)

;; Execute an approved proposal after timelock
(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (guild-id (get guild-id proposal))
    (guild (unwrap! (map-get? guilds { guild-id: guild-id }) ERR-GUILD-NOT-FOUND))
    (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
    (for-pct (if (> total-votes u0)
      (/ (* (get votes-for proposal) u100) total-votes)
      u0
    ))
  )
    (asserts! (not (get executed proposal)) ERR-ALREADY-EXECUTED)
    (asserts! (not (get cancelled proposal)) ERR-PROPOSAL-EXPIRED)
    (asserts! (>= block-height (get execute-after proposal)) ERR-TIMELOCK-PENDING)
    (asserts! (>= for-pct QUORUM-THRESHOLD) ERR-QUORUM-NOT-MET)
    (asserts! (<= (get amount proposal) (get treasury guild)) ERR-INSUFFICIENT-FUNDS)

    ;; Transfer funds from contract treasury to recipient
    (try! (as-contract (stx-transfer?
      (get amount proposal)
      tx-sender
      (get recipient proposal)
    )))

    ;; Deduct from guild treasury
    (map-set guilds { guild-id: guild-id }
      (merge guild { treasury: (- (get treasury guild) (get amount proposal)) })
    )

    ;; Mark executed
    (map-set proposals { proposal-id: proposal-id }
      (merge proposal { executed: true })
    )

    (ok true)
  )
)

;; Cancel a proposal (proposer or founder only, before voting ends)
(define-public (cancel-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (guild-id (get guild-id proposal))
    (guild (unwrap! (map-get? guilds { guild-id: guild-id }) ERR-GUILD-NOT-FOUND))
  )
    (asserts!
      (or
        (is-eq tx-sender (get proposer proposal))
        (is-eq tx-sender (get founder guild))
      )
      ERR-NOT-AUTHORIZED
    )
    (asserts! (not (get executed proposal)) ERR-ALREADY-EXECUTED)
    (asserts! (not (get cancelled proposal)) ERR-PROPOSAL-EXPIRED)

    (map-set proposals { proposal-id: proposal-id }
      (merge proposal { cancelled: true })
    )
    (ok true)
  )
)

;; ============================================================
;; REPUTATION NFT (SIP-009 compatible)
;; ============================================================

;; Mint a reputation NFT to a player (officers can award)
(define-public (mint-reputation-nft
  (guild-id uint)
  (recipient principal)
  (score uint)
  (category (string-ascii 32))
  (uri (string-ascii 256))
)
  (let (
    (token-id (+ (var-get rep-token-nonce) u1))
  )
    (asserts! (is-guild-officer guild-id tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-guild-member guild-id recipient) ERR-NOT-MEMBER)

    ;; Mint NFT
    (try! (nft-mint? reputation-nft token-id recipient))

    ;; Store metadata
    (map-set rep-nft-data { token-id: token-id }
      {
        owner:     recipient,
        guild-id:  guild-id,
        score:     score,
        category:  category,
        issued-at: block-height,
        uri:       uri
      }
    )

    ;; Mint reputation fungible tokens proportional to score
    (try! (ft-mint? reputation-token (* score REPUTATION-MINT-RATE) recipient))
    (var-set total-rep-supply (+ (var-get total-rep-supply) (* score REPUTATION-MINT-RATE)))

    ;; Update aggregated player reputation
    (update-player-reputation recipient score)

    (var-set rep-token-nonce token-id)
    (ok token-id)
  )
)

;; Transfer a reputation NFT (SIP-009)
(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (let (
    (nft-data (unwrap! (map-get? rep-nft-data { token-id: token-id }) ERR-TOKEN-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get owner nft-data) sender) ERR-NOT-AUTHORIZED)

    (try! (nft-transfer? reputation-nft token-id sender recipient))

    (map-set rep-nft-data { token-id: token-id }
      (merge nft-data { owner: recipient })
    )
    (ok true)
  )
)

;; SIP-009 read-only
(define-read-only (get-last-token-id)
  (ok (var-get rep-token-nonce))
)

(define-read-only (get-token-uri (token-id uint))
  (match (map-get? rep-nft-data { token-id: token-id })
    d (ok (some (get uri d)))
    (err ERR-TOKEN-NOT-FOUND)
  )
)

(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? reputation-nft token-id))
)

;; ============================================================
;; PLAY-TO-EARN & DIVIDEND DISTRIBUTION
;; ============================================================

;; Record a play-to-earn reward for a member (oracle-driven, officer submits)
(define-public (record-play-reward (guild-id uint) (player principal) (reward-amount uint))
  (let (
    (guild (unwrap! (map-get? guilds { guild-id: guild-id }) ERR-GUILD-NOT-FOUND))
    (membership (unwrap! (map-get? guild-members { guild-id: guild-id, member: player }) ERR-NOT-MEMBER))
  )
    (asserts! (is-guild-officer guild-id tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> reward-amount u0) ERR-INVALID-AMOUNT)

    ;; Mint reputation tokens as reward
    (try! (ft-mint? reputation-token reward-amount player))
    (var-set total-rep-supply (+ (var-get total-rep-supply) reward-amount))

    ;; Update contributions
    (map-set guild-members { guild-id: guild-id, member: player }
      (merge membership { contributions: (+ (get contributions membership) reward-amount) })
    )

    ;; Update player stats
    (match (map-get? player-reputation { player: player })
      r (map-set player-reputation { player: player }
          (merge r { rewards-claimed: (+ (get rewards-claimed r) u1) }))
      false
    )

    (update-player-reputation player (/ reward-amount u100))
    (ok true)
  )
)

;; Distribute guild treasury dividends to members
;; NOTE: In production, dividend distribution per member would require off-chain
;; computation and batch execution due to Clarity's lack of iteration.
;; This function distributes a single member's share.
(define-public (claim-dividend (guild-id uint))
  (let (
    (guild (unwrap! (map-get? guilds { guild-id: guild-id }) ERR-GUILD-NOT-FOUND))
    (membership (unwrap! (map-get? guild-members { guild-id: guild-id, member: tx-sender }) ERR-NOT-MEMBER))
    (gov-balance (ft-get-balance governance-token tx-sender))
    (total-gov (var-get total-gov-supply))
    (dividend-pool (/ (get treasury guild) u10))  ;; 10% of treasury per interval
    (member-share (if (> total-gov u0)
      (/ (* dividend-pool gov-balance) total-gov)
      u0
    ))
    (blocks-since-last (- block-height (get last-dividend-block guild)))
  )
    (asserts! (>= blocks-since-last DIVIDEND-INTERVAL) ERR-TIMELOCK-PENDING)
    (asserts! (> member-share u0) ERR-INSUFFICIENT-FUNDS)
    (asserts! (<= member-share (get treasury guild)) ERR-INSUFFICIENT-FUNDS)

    ;; Transfer dividend from contract to member
    (try! (as-contract (stx-transfer? member-share tx-sender tx-sender)))

    ;; Deduct from guild treasury and update dividend timestamp
    (map-set guilds { guild-id: guild-id }
      (merge guild {
        treasury: (- (get treasury guild) member-share),
        last-dividend-block: block-height
      })
    )

    (ok member-share)
  )
)

;; ============================================================
;; CONSULTING FEES (Reputation-Based Income)
;; ============================================================

;; Set a consulting fee arrangement
(define-public (set-consulting-fee (client principal) (fee-per-session uint))
  (let (
    (rep (default-to
      { total-score: u0, guilds-led: u0, proposals-made: u0, rewards-claimed: u0 }
      (map-get? player-reputation { player: tx-sender })
    ))
  )
    ;; Minimum reputation score of 100 required to offer consulting
    (asserts! (>= (get total-score rep) u100) ERR-NOT-AUTHORIZED)
    (asserts! (> fee-per-session u0) ERR-INVALID-AMOUNT)

    (map-set consulting-fees { consultant: tx-sender, client: client }
      {
        fee-per-session: fee-per-session,
        sessions:        u0,
        active:          true
      }
    )
    (ok true)
  )
)

;; Pay a consulting fee
(define-public (pay-consulting-fee (consultant principal))
  (let (
    (arrangement (unwrap!
      (map-get? consulting-fees { consultant: consultant, client: tx-sender })
      ERR-NOT-AUTHORIZED
    ))
  )
    (asserts! (get active arrangement) ERR-NOT-AUTHORIZED)

    (try! (stx-transfer? (get fee-per-session arrangement) tx-sender consultant))

    (map-set consulting-fees { consultant: consultant, client: tx-sender }
      (merge arrangement { sessions: (+ (get sessions arrangement) u1) })
    )

    ;; Reward consultant with reputation tokens
    (try! (ft-mint? reputation-token u50 consultant))
    (var-set total-rep-supply (+ (var-get total-rep-supply) u50))

    (ok true)
  )
)

;; ============================================================
;; ADMIN
;; ============================================================

;; Update platform fee (owner only)
(define-public (set-platform-fee (new-fee-bps uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-fee-bps u1000) ERR-INVALID-AMOUNT)  ;; Max 10%
    (var-set platform-fee-bps new-fee-bps)
    (ok true)
  )
)

;; Deactivate a guild (owner or founder)
(define-public (deactivate-guild (guild-id uint))
  (let (
    (guild (unwrap! (map-get? guilds { guild-id: guild-id }) ERR-GUILD-NOT-FOUND))
  )
    (asserts!
      (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get founder guild)))
      ERR-NOT-AUTHORIZED
    )
    (map-set guilds { guild-id: guild-id }
      (merge guild { active: false })
    )
    (ok true)
  )
)

;; ============================================================
;; READ-ONLY QUERIES
;; ============================================================

(define-read-only (get-guild (guild-id uint))
  (map-get? guilds { guild-id: guild-id })
)

(define-read-only (get-membership (guild-id uint) (member principal))
  (map-get? guild-members { guild-id: guild-id, member: member })
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-player-reputation (player principal))
  (map-get? player-reputation { player: player })
)

(define-read-only (get-rep-nft (token-id uint))
  (map-get? rep-nft-data { token-id: token-id })
)

(define-read-only (get-governance-balance (who principal))
  (ft-get-balance governance-token who)
)

(define-read-only (get-reputation-balance (who principal))
  (ft-get-balance reputation-token who)
)

(define-read-only (get-guild-count)
  (var-get guild-nonce)
)

(define-read-only (get-proposal-count)
  (var-get proposal-nonce)
)

(define-read-only (get-total-gov-supply)
  (var-get total-gov-supply)
)

(define-read-only (get-total-rep-supply)
  (var-get total-rep-supply)
)
