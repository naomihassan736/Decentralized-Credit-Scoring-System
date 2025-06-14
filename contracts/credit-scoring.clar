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