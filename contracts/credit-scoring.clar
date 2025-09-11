(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-score (err u103))
(define-constant err-invalid-weight (err u104))
(define-constant err-invalid-address (err u105))
(define-constant err-unauthorized (err u106))

(define-constant min-score u0)
(define-constant max-score u850)
(define-constant default-score u500)
(define-constant min-weight u1)
(define-constant max-weight u10)

(define-data-var score-algorithm-version uint u1)
(define-data-var total-users uint u0)
(define-data-var paused bool false)


(define-constant err-insufficient-stake (err u110))
(define-constant err-verification-exists (err u111))
(define-constant err-not-in-verification (err u112))
(define-constant err-verification-expired (err u113))
(define-constant err-invalid-verification (err u114))
(define-constant err-already-verified (err u115))
(define-constant err-insufficient-balance (err u116))

(define-constant min-stake-amount u1000)
(define-constant verification-period u144)
(define-constant challenge-period u72)
(define-constant reward-percentage u20)
(define-constant slash-percentage u50)

(define-data-var verification-counter uint u0)
(define-data-var total-staked uint u0)
(define-data-var reward-pool uint u0)

(define-map verification-requests
  { verification-id: uint }
  {
    reporter: principal,
    target-address: principal,
    proposed-score: uint,
    stake-amount: uint,
    timestamp: uint,
    status: (string-ascii 20),
    validators: (list 10 principal),
    support-votes: uint,
    challenge-votes: uint,
    final-score: (optional uint)
  }
)

(define-map reporter-stakes
  { reporter: principal }
  {
    total-staked: uint,
    active-verifications: uint,
    reputation-score: uint,
    slash-count: uint
  }
)

(define-map validator-participations
  { validator: principal, verification-id: uint }
  {
    vote: (string-ascii 10),
    timestamp: uint,
    stake-amount: uint
  }
)

(define-map verification-challenges
  { verification-id: uint, challenger: principal }
  {
    challenge-reason: (string-ascii 200),
    evidence-hash: (string-ascii 64),
    timestamp: uint,
    resolved: bool
  }
)

(define-map validator-rewards
  { validator: principal }
  {
    total-earned: uint,
    successful-validations: uint,
    failed-validations: uint,
    reputation: uint
  }
)

(define-map user-scores
  { address: principal }
  {
    score: uint,
    last-updated: uint,
    history-count: uint
  }
)

(define-map user-financial-history
  { address: principal, tx-id: uint }
  {
    amount: uint,
    timestamp: uint,
    tx-type: (string-ascii 20),
    counterparty: (optional principal)
  }
)

(define-map score-factors
  { factor-id: uint }
  {
    name: (string-ascii 50),
    weight: uint,
    active: bool
  }
)

(define-map authorized-reporters principal bool)

(define-map user-factor-scores
  { address: principal, factor-id: uint }
  { score: uint }
)

(define-read-only (get-credit-score (address principal))
  (match (map-get? user-scores { address: address })
    score-data (ok (get score score-data))
    (ok default-score)
  )
)

(define-read-only (get-user-data (address principal))
  (match (map-get? user-scores { address: address })
    score-data (ok score-data)
    (err err-not-found)
  )
)

(define-read-only (get-factor (factor-id uint))
  (match (map-get? score-factors { factor-id: factor-id })
    factor (ok factor)
    (err err-not-found)
  )
)

(define-read-only (get-factor-score (address principal) (factor-id uint))
  (match (map-get? user-factor-scores { address: address, factor-id: factor-id })
    factor-score (ok (get score factor-score))
    (err err-not-found)
  )
)

(define-read-only (is-authorized-reporter (reporter principal))
  (default-to false (map-get? authorized-reporters reporter))
)

(define-read-only (get-total-users)
  (var-get total-users)
)

(define-read-only (get-algorithm-version)
  (var-get score-algorithm-version)
)

(define-public (register-user)
  (let ((address tx-sender))
    (match (map-get? user-scores { address: address })
      score-data (err err-already-exists)
      (begin
        (map-set user-scores
          { address: address }
          {
            score: default-score,
            last-updated: stacks-block-height,
            history-count: u0
          }
        )
        (var-set total-users (+ (var-get total-users) u1))
        (ok true)
      )
    )
  )
)

(define-public (add-financial-record (address principal) (amount uint) (tx-type (string-ascii 20)) (counterparty (optional principal)))
  (if (or (is-eq tx-sender contract-owner) (is-authorized-reporter tx-sender))
    (let (
      (user-data (unwrap! (map-get? user-scores { address: address }) (err err-not-found)))
      (history-count (get history-count user-data))
      (new-history-count (+ history-count u1))
    )
      (map-set user-financial-history
        { address: address, tx-id: history-count }
        {
          amount: amount,
          timestamp: stacks-block-height,
          tx-type: tx-type,
          counterparty: counterparty
        }
      )
      (map-set user-scores
        { address: address }
        (merge user-data { history-count: new-history-count })
      )
      (ok new-history-count)
    )
    (err err-unauthorized)
  )
)

(define-public (update-credit-score (address principal) (new-score uint))
  (if (or (is-eq tx-sender contract-owner) (is-authorized-reporter tx-sender))
    (if (and (>= new-score min-score) (<= new-score max-score))
      (match (map-get? user-scores { address: address })
        score-data 
        (begin
          (map-set user-scores
            { address: address }
            (merge score-data { 
              score: new-score,
              last-updated: stacks-block-height
            })
          )
          (ok new-score)
        )
        (err err-not-found)
      )
      (err err-invalid-score)
    )
    (err err-unauthorized)
  )
)

(define-public (add-score-factor (factor-id uint) (name (string-ascii 50)) (weight uint))
  (if (is-eq tx-sender contract-owner)
    (if (and (>= weight min-weight) (<= weight max-weight))
      (begin
        (map-set score-factors
          { factor-id: factor-id }
          {
            name: name,
            weight: weight,
            active: true
          }
        )
        (ok factor-id)
      )
      (err err-invalid-weight)
    )
    (err err-owner-only)
  )
)

(define-public (update-factor-score (address principal) (factor-id uint) (score uint))
  (if (or (is-eq tx-sender contract-owner) (is-authorized-reporter tx-sender))
    (if (and (>= score min-score) (<= score max-score))
      (begin
        (map-set user-factor-scores
          { address: address, factor-id: factor-id }
          { score: score }
        )
        (ok score)
      )
      (err err-invalid-score)
    )
    (err err-unauthorized)
  )
)

(define-public (authorize-reporter (reporter principal))
  (if (is-eq tx-sender contract-owner)
    (begin
      (map-set authorized-reporters reporter true)
      (ok true)
    )
    (err err-owner-only)
  )
)

(define-public (revoke-reporter (reporter principal))
  (if (is-eq tx-sender contract-owner)
    (begin
      (map-delete authorized-reporters reporter)
      (ok true)
    )
    (err err-owner-only)
  )
)

(define-public (update-algorithm-version (new-version uint))
  (if (is-eq tx-sender contract-owner)
    (begin
      (var-set score-algorithm-version new-version)
      (ok new-version)
    )
    (err err-owner-only)
  )
)

(define-public (set-paused (paused-state bool))
  (if (is-eq tx-sender contract-owner)
    (begin
      (var-set paused paused-state)
      (ok paused-state)
    )
    (err err-owner-only)
  )
)

(define-public (calculate-score (address principal))
  (if (var-get paused)
    (err err-unauthorized)
    (let (
      (user-data (unwrap! (map-get? user-scores { address: address }) (err err-not-found)))
      (current-score (get score user-data))
    )
      (ok current-score)
    )
  )
)


(define-constant dispute-review-period u144)
(define-constant err-no-active-dispute (err u107))
(define-constant err-dispute-exists (err u108))

(define-map score-disputes
  { address: principal }
  {
    dispute-id: uint,
    old-score: uint,
    reason: (string-ascii 100),
    timestamp: uint,
    resolved: bool,
    resolver: (optional principal)
  }
)

(define-data-var dispute-counter uint u0)

(define-public (file-dispute (reason (string-ascii 100)))
  (let (
    (current-score (unwrap! (get-credit-score tx-sender) (err err-not-found)))
    (dispute-id (+ (var-get dispute-counter) u1))
  )
    (asserts! (is-none (map-get? score-disputes { address: tx-sender })) (err err-dispute-exists))
    (map-set score-disputes
      { address: tx-sender }
      {
        dispute-id: dispute-id,
        old-score: current-score,
        reason: reason,
        timestamp: stacks-block-height,
        resolved: false,
        resolver: none
      }
    )
    (var-set dispute-counter dispute-id)
    (ok dispute-id)
  )
)

(define-public (resolve-dispute (user principal) (new-score uint))
  (let ((dispute (unwrap! (map-get? score-disputes { address: user }) (err err-no-active-dispute)))
  )
    (asserts! (or (is-eq tx-sender contract-owner) (is-authorized-reporter tx-sender)) (err err-unauthorized))
    (asserts! (not (get resolved dispute)) (err err-no-active-dispute))
    (try! (update-credit-score user new-score))
    (map-set score-disputes
      { address: user }
      (merge dispute {
        resolved: true,
        resolver: (some tx-sender)
      })
    )
    (ok true)
  )
)


(define-map improvement-recommendations
  { score-range: uint }
  {
    recommendation: (string-ascii 200),
    priority: uint
  }
)

(define-constant score-ranges (list u300 u500 u700))

(define-public (add-recommendation (score-range uint) (recommendation (string-ascii 200)) (priority uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
    (map-set improvement-recommendations
      { score-range: score-range }
      {
        recommendation: recommendation,
        priority: priority
      }
    )
    (ok true)
  )
)

;; (define-read-only (get-recommendations (address principal))
;;   (let (
;;     (current-score (unwrap! (get-credit-score address) (err err-not-found)))
;;     ;; (applicable-range (fold get-applicable-range score-ranges current-score))
;;   )
;;     (match (map-get? improvement-recommendations { score-range: applicable-range })
;;       recommendation (ok recommendation)
;;       (err err-not-found)
;;     )
;;   )
;; )


(define-constant err-invalid-limit (err u109))
(define-constant max-history-limit u50)

(define-map score-history
  { address: principal, entry-id: uint }
  {
    old-score: uint,
    new-score: uint,
    timestamp: uint,
    changed-by: principal,
    reason: (string-ascii 50)
  }
)

(define-map user-history-counters
  { address: principal }
  { count: uint }
)

(define-private (get-history-count (address principal))
  (default-to u0 (get count (map-get? user-history-counters { address: address })))
)

(define-private (increment-history-count (address principal))
  (let ((current-count (get-history-count address)))
    (map-set user-history-counters
      { address: address }
      { count: (+ current-count u1) }
    )
    (+ current-count u1)
  )
)

(define-private (record-score-change (address principal) (old-score uint) (new-score uint) (reason (string-ascii 50)))
  (let ((entry-id (increment-history-count address)))
    (map-set score-history
      { address: address, entry-id: entry-id }
      {
        old-score: old-score,
        new-score: new-score,
        timestamp: stacks-block-height,
        changed-by: tx-sender,
        reason: reason
      }
    )
    entry-id
  )
)

(define-public (update-credit-score-with-reason (address principal) (new-score uint) (reason (string-ascii 50)))
  (if (or (is-eq tx-sender contract-owner) (is-authorized-reporter tx-sender))
    (if (and (>= new-score min-score) (<= new-score max-score))
      (match (map-get? user-scores { address: address })
        score-data 
        (let ((old-score (get score score-data)))
          (map-set user-scores
            { address: address }
            (merge score-data { 
              score: new-score,
              last-updated: stacks-block-height
            })
          )
          (record-score-change address old-score new-score reason)
          (ok new-score)
        )
        (err err-not-found)
      )
      (err err-invalid-score)
    )
    (err err-unauthorized)
  )
)

(define-read-only (get-score-history-entry (address principal) (entry-id uint))
  (match (map-get? score-history { address: address, entry-id: entry-id })
    history-entry (ok history-entry)
    (err err-not-found)
  )
)



(define-read-only (get-score-trend (address principal) (periods uint))
  (if (and (> periods u0) (<= periods u10))
    (let (
      (current-score (unwrap! (get-credit-score address) (err err-not-found)))
      (total-count (get-history-count address))
    )
      (if (>= total-count periods)
        (let ((old-entry (map-get? score-history { address: address, entry-id: (- total-count periods) })))
          (match old-entry
            entry (ok {
              current-score: current-score,
              previous-score: (get old-score entry),
              change: (if (>= current-score (get old-score entry)) 
                       (- current-score (get old-score entry))
                       (- (get old-score entry) current-score)),
              trend: (if (> current-score (get old-score entry)) "up" 
                     (if (< current-score (get old-score entry)) "down" "stable"))
            })
            (ok {
              current-score: current-score,
              previous-score: current-score,
              change: u0,
              trend: "stable"
            })
          )
        )
        (ok {
          current-score: current-score,
          previous-score: current-score,
          change: u0,
          trend: "insufficient-data"
        })
      )
    )
    (err err-invalid-limit)
  )
)

(define-read-only (get-user-history-stats (address principal))
  (let (
    (total-entries (get-history-count address))
    (current-score (unwrap! (get-credit-score address) (err err-not-found)))
  )
    (ok {
      total-score-changes: total-entries,
      current-score: current-score,
      first-recorded: (if (> total-entries u0) 
                       (get timestamp (unwrap-panic (map-get? score-history { address: address, entry-id: u1 })))
                       u0),
      last-updated: (if (> total-entries u0)
                     (get timestamp (unwrap-panic (map-get? score-history { address: address, entry-id: total-entries })))
                     u0)
    })
  )
)


(define-read-only (get-verification-request (verification-id uint))
  (match (map-get? verification-requests { verification-id: verification-id })
    verification (ok verification)
    (err err-not-found)
  )
)

(define-read-only (get-reporter-stake (reporter principal))
  (default-to 
    { total-staked: u0, active-verifications: u0, reputation-score: u100, slash-count: u0 }
    (map-get? reporter-stakes { reporter: reporter })
  )
)

(define-read-only (get-validator-participation (validator principal) (verification-id uint))
  (match (map-get? validator-participations { validator: validator, verification-id: verification-id })
    participation (ok participation)
    (err err-not-found)
  )
)

(define-read-only (get-verification-challenge (verification-id uint) (challenger principal))
  (match (map-get? verification-challenges { verification-id: verification-id, challenger: challenger })
    challenge (ok challenge)
    (err err-not-found)
  )
)

(define-read-only (get-validator-rewards (validator principal))
  (default-to 
    { total-earned: u0, successful-validations: u0, failed-validations: u0, reputation: u100 }
    (map-get? validator-rewards { validator: validator })
  )
)

(define-read-only (get-staking-stats)
  (ok {
    total-staked: (var-get total-staked),
    reward-pool: (var-get reward-pool),
    active-verifications: (var-get verification-counter)
  })
)

(define-public (stake-for-verification (target-address principal) (proposed-score uint) (stake-amount uint))
  (let (
    (verification-id (+ (var-get verification-counter) u1))
    (reporter-data (get-reporter-stake tx-sender))
  )
    (asserts! (>= stake-amount min-stake-amount) (err err-insufficient-stake))
    (asserts! (and (>= proposed-score min-score) (<= proposed-score max-score)) (err err-invalid-score))
    (asserts! (is-none (map-get? verification-requests { verification-id: verification-id })) (err err-verification-exists))
    
    (map-set verification-requests
      { verification-id: verification-id }
      {
        reporter: tx-sender,
        target-address: target-address,
        proposed-score: proposed-score,
        stake-amount: stake-amount,
        timestamp: stacks-block-height,
        status: "pending",
        validators: (list),
        support-votes: u0,
        challenge-votes: u0,
        final-score: none
      }
    )
    
    (map-set reporter-stakes
      { reporter: tx-sender }
      {
        total-staked: (+ (get total-staked reporter-data) stake-amount),
        active-verifications: (+ (get active-verifications reporter-data) u1),
        reputation-score: (get reputation-score reporter-data),
        slash-count: (get slash-count reporter-data)
      }
    )
    
    (var-set verification-counter verification-id)
    (var-set total-staked (+ (var-get total-staked) stake-amount))
    (ok verification-id)
  )
)

(define-public (participate-in-verification (verification-id uint) (vote (string-ascii 10)) (validator-stake uint))
  (let (
    (verification (unwrap! (map-get? verification-requests { verification-id: verification-id }) (err err-not-found)))
    (current-validators (get validators verification))
  )
    (asserts! (>= validator-stake min-stake-amount) (err err-insufficient-stake))
    (asserts! (is-eq (get status verification) "pending") (err err-not-in-verification))
    (asserts! (< (+ (get timestamp verification) verification-period) stacks-block-height) (err err-verification-expired))
    (asserts! (is-none (map-get? validator-participations { validator: tx-sender, verification-id: verification-id })) (err err-already-verified))
    
    (map-set validator-participations
      { validator: tx-sender, verification-id: verification-id }
      {
        vote: vote,
        timestamp: stacks-block-height,
        stake-amount: validator-stake
      }
    )
    
    (if (is-eq vote "support")
      (map-set verification-requests
        { verification-id: verification-id }
        (merge verification { 
          support-votes: (+ (get support-votes verification) u1),
          validators: (unwrap-panic (as-max-len? (append current-validators tx-sender) u10))
        })
      )
      (map-set verification-requests
        { verification-id: verification-id }
        (merge verification { 
          challenge-votes: (+ (get challenge-votes verification) u1),
          validators: (unwrap-panic (as-max-len? (append current-validators tx-sender) u10))
        })
      )
    )
    
    (ok true)
  )
)

(define-public (challenge-verification (verification-id uint) (reason (string-ascii 200)) (evidence-hash (string-ascii 64)))
  (let (
    (verification (unwrap! (map-get? verification-requests { verification-id: verification-id }) (err err-not-found)))
  )
    (asserts! (is-eq (get status verification) "pending") (err err-not-in-verification))
    (asserts! (< (+ (get timestamp verification) challenge-period) stacks-block-height) (err err-verification-expired))
    (asserts! (is-none (map-get? verification-challenges { verification-id: verification-id, challenger: tx-sender })) (err err-already-verified))
    
    (map-set verification-challenges
      { verification-id: verification-id, challenger: tx-sender }
      {
        challenge-reason: reason,
        evidence-hash: evidence-hash,
        timestamp: stacks-block-height,
        resolved: false
      }
    )
    
    (ok true)
  )
)

(define-public (resolve-verification (verification-id uint))
  (let (
    (verification (unwrap! (map-get? verification-requests { verification-id: verification-id }) (err err-not-found)))
    (support-votes (get support-votes verification))
    (challenge-votes (get challenge-votes verification))
    (reporter (get reporter verification))
    (target-address (get target-address verification))
    (proposed-score (get proposed-score verification))
    (stake-amount (get stake-amount verification))
    (reporter-data (get-reporter-stake reporter))
  )
    (asserts! (is-eq (get status verification) "pending") (err err-invalid-verification))
    (asserts! (>= (+ (get timestamp verification) verification-period) stacks-block-height) (err err-verification-expired))
    
    (if (> support-votes challenge-votes)
      (begin
        (try! (update-credit-score target-address proposed-score))
        (map-set verification-requests
          { verification-id: verification-id }
          (merge verification { 
            status: "approved",
            final-score: (some proposed-score)
          })
        )
        (map-set reporter-stakes
          { reporter: reporter }
          (merge reporter-data {
            reputation-score: (+ (get reputation-score reporter-data) u10),
            active-verifications: (- (get active-verifications reporter-data) u1)
          })
        )
        (var-set reward-pool (+ (var-get reward-pool) (/ (* stake-amount reward-percentage) u100)))
        (ok "approved")
      )
      (begin
        (map-set verification-requests
          { verification-id: verification-id }
          (merge verification { 
            status: "rejected",
            final-score: none
          })
        )
        (let ((slash-amount (/ (* stake-amount slash-percentage) u100)))
          (map-set reporter-stakes
            { reporter: reporter }
            (merge reporter-data {
              total-staked: (- (get total-staked reporter-data) slash-amount),
              reputation-score: (if (>= (get reputation-score reporter-data) u20) 
                                 (- (get reputation-score reporter-data) u20) 
                                 u0),
              slash-count: (+ (get slash-count reporter-data) u1),
              active-verifications: (- (get active-verifications reporter-data) u1)
            })
          )
          (var-set total-staked (- (var-get total-staked) slash-amount))
          (var-set reward-pool (+ (var-get reward-pool) slash-amount))
        )
        (ok "rejected")
      )
    )
  )
)

(define-public (distribute-validator-rewards (verification-id uint))
  (let (
    (verification (unwrap! (map-get? verification-requests { verification-id: verification-id }) (err err-not-found)))
    (validators (get validators verification))
    (is-approved (is-eq (get status verification) "approved"))
    (total-reward (/ (var-get reward-pool) (len validators)))
  )
    (asserts! (not (is-eq (get status verification) "pending")) (err err-not-in-verification))
    (asserts! (> (len validators) u0) (err err-not-found))
    
    (fold distribute-individual-reward validators { reward-amount: total-reward, verification-id: verification-id, approved: is-approved })
    (var-set reward-pool (- (var-get reward-pool) (* total-reward (len validators))))
    (ok true)
  )
)

(define-private (distribute-individual-reward (validator principal) (context { reward-amount: uint, verification-id: uint, approved: bool }))
  (let (
    (participation (map-get? validator-participations { validator: validator, verification-id: (get verification-id context) }))
    (current-rewards (get-validator-rewards validator))
    (reward-amount (get reward-amount context))
    (is-approved (get approved context))
  )
    (match participation
      part-data 
      (let (
        (vote (get vote part-data))
        (correct-vote (or (and is-approved (is-eq vote "support")) (and (not is-approved) (is-eq vote "challenge"))))
      )
        (if correct-vote
          (map-set validator-rewards
            { validator: validator }
            {
              total-earned: (+ (get total-earned current-rewards) reward-amount),
              successful-validations: (+ (get successful-validations current-rewards) u1),
              failed-validations: (get failed-validations current-rewards),
              reputation: (+ (get reputation current-rewards) u5)
            }
          )
          (map-set validator-rewards
            { validator: validator }
            {
              total-earned: (get total-earned current-rewards),
              successful-validations: (get successful-validations current-rewards),
              failed-validations: (+ (get failed-validations current-rewards) u1),
              reputation: (if (>= (get reputation current-rewards) u5) 
                           (- (get reputation current-rewards) u5) 
                           u0)
            }
          )
        )
      )
      false
    )
    context
  )
)

(define-public (withdraw-stake (amount uint))
  (let (
    (reporter-data (get-reporter-stake tx-sender))
    (available-stake (- (get total-staked reporter-data) (* (get active-verifications reporter-data) min-stake-amount)))
  )
    (asserts! (<= amount available-stake) (err err-insufficient-balance))
    (asserts! (> amount u0) (err err-invalid-verification))
    
    (map-set reporter-stakes
      { reporter: tx-sender }
      (merge reporter-data {
        total-staked: (- (get total-staked reporter-data) amount)
      })
    )
    
    (var-set total-staked (- (var-get total-staked) amount))
    (ok amount)
  )
)

;; Social Credit Vouching & Endorsement System
(define-constant err-invalid-voucher (err u117))
(define-constant err-vouch-exists (err u118))
(define-constant err-self-vouch (err u119))
(define-constant err-voucher-limit-exceeded (err u120))
(define-constant err-insufficient-voucher-score (err u121))
(define-constant err-vouch-not-found (err u122))
(define-constant err-vouch-expired (err u123))
(define-constant err-already-vouched (err u124))

;; Vouching system constants
(define-constant min-voucher-score u650) ;; Minimum score to be a voucher
(define-constant max-vouches-per-user u5) ;; Maximum active vouches per voucher
(define-constant vouch-duration u1008) ;; Vouch active period (about 1 week in blocks)
(define-constant vouch-reputation-stake u50) ;; Reputation points staked per vouch
(define-constant score-boost-amount u25) ;; Score boost for vouched users
(define-constant voucher-reward-amount u10) ;; Reputation reward for successful vouches
(define-constant voucher-penalty-amount u30) ;; Reputation penalty for failed vouches

;; Vouching system data variables
(define-data-var vouch-counter uint u0)
(define-data-var total-active-vouches uint u0)

;; Track individual vouching relationships
(define-map credit-vouches
  { vouch-id: uint }
  {
    voucher: principal,
    vouched-user: principal,
    vouch-amount: uint, ;; Reputation staked
    start-block: uint,
    end-block: uint,
    status: (string-ascii 20), ;; "active", "completed", "defaulted"
    score-boost-applied: bool,
    voucher-benefit: uint ;; Benefit voucher has received from this vouch
  }
)

;; Track voucher statistics and limits
(define-map voucher-profiles
  { voucher: principal }
  {
    total-vouches-given: uint,
    active-vouches: uint,
    successful-vouches: uint,
    failed-vouches: uint,
    reputation-staked: uint,
    voucher-reputation: uint, ;; Separate vouching reputation
    total-voucher-rewards: uint
  }
)

;; Track users who have been vouched for
(define-map vouched-users
  { user: principal }
  {
    current-vouchers: (list 5 principal),
    total-vouches-received: uint,
    active-vouches: uint,
    score-boosts-active: uint,
    vouching-history-count: uint
  }
)

;; Track vouching relationships for easy lookup
(define-map user-vouch-relationships
  { voucher: principal, vouched-user: principal }
  { vouch-id: uint, active: bool }
)

;; Read-only functions for vouching system
(define-read-only (get-vouch-details (vouch-id uint))
  (match (map-get? credit-vouches { vouch-id: vouch-id })
    vouch (ok vouch)
    (err err-vouch-not-found)
  )
)

(define-read-only (get-voucher-profile (voucher principal))
  (default-to 
    { 
      total-vouches-given: u0, 
      active-vouches: u0, 
      successful-vouches: u0, 
      failed-vouches: u0, 
      reputation-staked: u0, 
      voucher-reputation: u100,
      total-voucher-rewards: u0
    }
    (map-get? voucher-profiles { voucher: voucher })
  )
)

(define-read-only (get-vouched-user-status (user principal))
  (default-to 
    { 
      current-vouchers: (list), 
      total-vouches-received: u0, 
      active-vouches: u0, 
      score-boosts-active: u0,
      vouching-history-count: u0
    }
    (map-get? vouched-users { user: user })
  )
)

(define-read-only (get-vouch-relationship (voucher principal) (vouched-user principal))
  (match (map-get? user-vouch-relationships { voucher: voucher, vouched-user: vouched-user })
    relationship (ok relationship)
    (err err-vouch-not-found)
  )
)

(define-read-only (is-eligible-voucher (potential-voucher principal))
  (let (
    (voucher-score (unwrap! (get-credit-score potential-voucher) (err err-not-found)))
    (voucher-profile (get-voucher-profile potential-voucher))
  )
    (ok {
      meets-score-requirement: (>= voucher-score min-voucher-score),
      within-vouch-limit: (< (get active-vouches voucher-profile) max-vouches-per-user),
      voucher-reputation: (get voucher-reputation voucher-profile)
    })
  )
)

(define-read-only (calculate-vouch-benefit (vouched-user principal))
  (let (
    (current-score (unwrap! (get-credit-score vouched-user) (err err-not-found)))
    (vouched-status (get-vouched-user-status vouched-user))
    (active-boosts (get score-boosts-active vouched-status))
  )
    (ok {
      base-score: current-score,
      total-boost: (* active-boosts score-boost-amount),
      effective-score: (+ current-score (* active-boosts score-boost-amount))
    })
  )
)

;; Create a vouch for another user
(define-public (vouch-for-user (vouched-user principal) (reputation-stake uint))
  (let (
    (vouch-id (+ (var-get vouch-counter) u1))
    (voucher-score (unwrap! (get-credit-score tx-sender) (err err-invalid-voucher)))
    (voucher-profile (get-voucher-profile tx-sender))
    (vouched-status (get-vouched-user-status vouched-user))
  )
    ;; Validation checks
    (asserts! (not (is-eq tx-sender vouched-user)) (err err-self-vouch))
    (asserts! (>= voucher-score min-voucher-score) (err err-insufficient-voucher-score))
    (asserts! (< (get active-vouches voucher-profile) max-vouches-per-user) (err err-voucher-limit-exceeded))
    (asserts! (>= reputation-stake vouch-reputation-stake) (err err-insufficient-stake))
    (asserts! (is-none (map-get? user-vouch-relationships { voucher: tx-sender, vouched-user: vouched-user })) (err err-already-vouched))
    
    ;; Create the vouch record
    (map-set credit-vouches
      { vouch-id: vouch-id }
      {
        voucher: tx-sender,
        vouched-user: vouched-user,
        vouch-amount: reputation-stake,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height vouch-duration),
        status: "active",
        score-boost-applied: true,
        voucher-benefit: u0
      }
    )
    
    ;; Update voucher profile
    (map-set voucher-profiles
      { voucher: tx-sender }
      {
        total-vouches-given: (+ (get total-vouches-given voucher-profile) u1),
        active-vouches: (+ (get active-vouches voucher-profile) u1),
        successful-vouches: (get successful-vouches voucher-profile),
        failed-vouches: (get failed-vouches voucher-profile),
        reputation-staked: (+ (get reputation-staked voucher-profile) reputation-stake),
        voucher-reputation: (get voucher-reputation voucher-profile),
        total-voucher-rewards: (get total-voucher-rewards voucher-profile)
      }
    )
    
    ;; Update vouched user status
    (map-set vouched-users
      { user: vouched-user }
      {
        current-vouchers: (unwrap-panic (as-max-len? (append (get current-vouchers vouched-status) tx-sender) u5)),
        total-vouches-received: (+ (get total-vouches-received vouched-status) u1),
        active-vouches: (+ (get active-vouches vouched-status) u1),
        score-boosts-active: (+ (get score-boosts-active vouched-status) u1),
        vouching-history-count: (+ (get vouching-history-count vouched-status) u1)
      }
    )
    
    ;; Create relationship mapping
    (map-set user-vouch-relationships
      { voucher: tx-sender, vouched-user: vouched-user }
      { vouch-id: vouch-id, active: true }
    )
    
    ;; Update global counters
    (var-set vouch-counter vouch-id)
    (var-set total-active-vouches (+ (var-get total-active-vouches) u1))
    
    (ok vouch-id)
  )
)

;; Complete a successful vouch (called when vouched user performs well)
(define-public (complete-vouch (vouch-id uint))
  (let (
    (vouch (unwrap! (map-get? credit-vouches { vouch-id: vouch-id }) (err err-vouch-not-found)))
    (voucher (get voucher vouch))
    (vouched-user (get vouched-user vouch))
    (voucher-profile (get-voucher-profile voucher))
    (vouched-status (get-vouched-user-status vouched-user))
  )
    ;; Only contract owner or authorized reporters can complete vouches
    (asserts! (or (is-eq tx-sender contract-owner) (is-authorized-reporter tx-sender)) (err err-unauthorized))
    (asserts! (is-eq (get status vouch) "active") (err err-invalid-verification))
    
    ;; Update vouch status
    (map-set credit-vouches
      { vouch-id: vouch-id }
      (merge vouch { 
        status: "completed",
        voucher-benefit: voucher-reward-amount
      })
    )
    
    ;; Reward the voucher
    (map-set voucher-profiles
      { voucher: voucher }
      (merge voucher-profile {
        active-vouches: (- (get active-vouches voucher-profile) u1),
        successful-vouches: (+ (get successful-vouches voucher-profile) u1),
        reputation-staked: (- (get reputation-staked voucher-profile) (get vouch-amount vouch)),
        voucher-reputation: (+ (get voucher-reputation voucher-profile) voucher-reward-amount),
        total-voucher-rewards: (+ (get total-voucher-rewards voucher-profile) voucher-reward-amount)
      })
    )
    
    ;; Update vouched user (remove boost but keep history)
    (map-set vouched-users
      { user: vouched-user }
      (merge vouched-status {
        active-vouches: (- (get active-vouches vouched-status) u1),
        score-boosts-active: (- (get score-boosts-active vouched-status) u1)
      })
    )
    
    ;; Update relationship
    (map-set user-vouch-relationships
      { voucher: voucher, vouched-user: vouched-user }
      { vouch-id: vouch-id, active: false }
    )
    
    (var-set total-active-vouches (- (var-get total-active-vouches) u1))
    (ok true)
  )
)

;; Penalize voucher for failed vouch (called when vouched user defaults)
(define-public (penalize-vouch (vouch-id uint))
  (let (
    (vouch (unwrap! (map-get? credit-vouches { vouch-id: vouch-id }) (err err-vouch-not-found)))
    (voucher (get voucher vouch))
    (vouched-user (get vouched-user vouch))
    (voucher-profile (get-voucher-profile voucher))
    (vouched-status (get-vouched-user-status vouched-user))
  )
    ;; Only contract owner or authorized reporters can penalize vouches
    (asserts! (or (is-eq tx-sender contract-owner) (is-authorized-reporter tx-sender)) (err err-unauthorized))
    (asserts! (is-eq (get status vouch) "active") (err err-invalid-verification))
    
    ;; Update vouch status
    (map-set credit-vouches
      { vouch-id: vouch-id }
      (merge vouch { status: "defaulted" })
    )
    
    ;; Penalize the voucher
    (map-set voucher-profiles
      { voucher: voucher }
      (merge voucher-profile {
        active-vouches: (- (get active-vouches voucher-profile) u1),
        failed-vouches: (+ (get failed-vouches voucher-profile) u1),
        reputation-staked: (- (get reputation-staked voucher-profile) (get vouch-amount vouch)),
        voucher-reputation: (if (>= (get voucher-reputation voucher-profile) voucher-penalty-amount)
                             (- (get voucher-reputation voucher-profile) voucher-penalty-amount)
                             u0)
      })
    )
    
    ;; Update vouched user (remove boost)
    (map-set vouched-users
      { user: vouched-user }
      (merge vouched-status {
        active-vouches: (- (get active-vouches vouched-status) u1),
        score-boosts-active: (- (get score-boosts-active vouched-status) u1)
      })
    )
    
    ;; Update relationship
    (map-set user-vouch-relationships
      { voucher: voucher, vouched-user: vouched-user }
      { vouch-id: vouch-id, active: false }
    )
    
    (var-set total-active-vouches (- (var-get total-active-vouches) u1))
    (ok true)
  )
)

;; Revoke an active vouch (voluntary withdrawal by voucher)
(define-public (revoke-vouch (vouched-user principal))
  (let (
    (relationship (unwrap! (map-get? user-vouch-relationships { voucher: tx-sender, vouched-user: vouched-user }) (err err-vouch-not-found)))
    (vouch-id (get vouch-id relationship))
    (vouch (unwrap! (map-get? credit-vouches { vouch-id: vouch-id }) (err err-vouch-not-found)))
    (voucher-profile (get-voucher-profile tx-sender))
    (vouched-status (get-vouched-user-status vouched-user))
  )
    (asserts! (get active relationship) (err err-vouch-expired))
    (asserts! (is-eq (get status vouch) "active") (err err-invalid-verification))
    
    ;; Update vouch status
    (map-set credit-vouches
      { vouch-id: vouch-id }
      (merge vouch { status: "revoked" })
    )
    
    ;; Update voucher profile (small penalty for revoking)
    (map-set voucher-profiles
      { voucher: tx-sender }
      (merge voucher-profile {
        active-vouches: (- (get active-vouches voucher-profile) u1),
        reputation-staked: (- (get reputation-staked voucher-profile) (get vouch-amount vouch)),
        voucher-reputation: (if (>= (get voucher-reputation voucher-profile) u5)
                             (- (get voucher-reputation voucher-profile) u5)
                             u0)
      })
    )
    
    ;; Update vouched user (remove boost)
    (map-set vouched-users
      { user: vouched-user }
      (merge vouched-status {
        active-vouches: (- (get active-vouches vouched-status) u1),
        score-boosts-active: (- (get score-boosts-active vouched-status) u1)
      })
    )
    
    ;; Update relationship
    (map-set user-vouch-relationships
      { voucher: tx-sender, vouched-user: vouched-user }
      (merge relationship { active: false })
    )
    
    (var-set total-active-vouches (- (var-get total-active-vouches) u1))
    (ok true)
  )
)



