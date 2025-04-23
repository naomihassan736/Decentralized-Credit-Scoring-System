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