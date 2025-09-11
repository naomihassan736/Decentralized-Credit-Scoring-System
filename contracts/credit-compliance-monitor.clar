;; Credit Compliance Monitor Contract
;; Monitors credit activities for regulatory compliance in decentralized credit scoring
;; Tracks patterns, flags suspicious activities, and provides compliance reporting

(define-constant CONTRACT-CREDIT-SCORING .credit-scoring)

;; Error constants
(define-constant ERR-OWNER-ONLY (err u200))
(define-constant ERR-NOT-FOUND (err u201))
(define-constant ERR-ALREADY-EXISTS (err u202))
(define-constant ERR-INVALID-PARAMS (err u203))
(define-constant ERR-UNAUTHORIZED (err u204))
(define-constant ERR-COMPLIANCE-VIOLATION (err u205))
(define-constant ERR-INSUFFICIENT-DATA (err u206))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-COMPLIANCE-SCORE u100)
(define-constant MIN-COMPLIANCE-SCORE u0)
(define-constant HIGH-RISK-THRESHOLD u30)
(define-constant MEDIUM-RISK-THRESHOLD u60)
(define-constant PATTERN-TRACKING-PERIOD u1008) ;; ~1 week in blocks

;; Data variables
(define-data-var total-monitored-users uint u0)
(define-data-var total-compliance-flags uint u0)
(define-data-var compliance-rules-version uint u1)

;; Compliance rules configuration
(define-map compliance-rules
  { rule-id: uint }
  {
    rule-name: (string-ascii 50),
    violation-threshold: uint,
    penalty-score: uint,
    auto-flag: bool,
    active: bool
  }
)

;; User compliance profiles
(define-map user-compliance-profiles
  { user: principal }
  {
    compliance-score: uint,
    risk-level: (string-ascii 10),
    total-violations: uint,
    last-review: uint,
    kyc-status: (string-ascii 20),
    monitoring-level: uint,
    flags-count: uint
  }
)

;; Activity pattern tracking
(define-map user-activity-patterns
  { user: principal, period: uint }
  {
    transaction-count: uint,
    score-changes: uint,
    high-value-activities: uint,
    suspicious-indicators: uint,
    rapid-changes: uint
  }
)

;; Compliance violations and flags
(define-map compliance-violations
  { user: principal, violation-id: uint }
  {
    rule-violated: uint,
    severity: (string-ascii 10),
    timestamp: uint,
    resolved: bool,
    description: (string-ascii 100),
    auto-flagged: bool
  }
)

;; User violation counters
(define-map user-violation-counters
  { user: principal }
  { count: uint }
)

;; Compliance reporting data
(define-map compliance-reports
  { report-id: uint }
  {
    report-type: (string-ascii 20),
    period-start: uint,
    period-end: uint,
    total-users: uint,
    flagged-users: uint,
    violations-count: uint,
    generated-by: principal,
    timestamp: uint
  }
)

(define-data-var report-counter uint u0)

;; Read-only functions

(define-read-only (get-user-compliance-profile (user principal))
  (default-to 
    {
      compliance-score: u100,
      risk-level: "low",
      total-violations: u0,
      last-review: u0,
      kyc-status: "pending",
      monitoring-level: u1,
      flags-count: u0
    }
    (map-get? user-compliance-profiles { user: user })
  )
)

(define-read-only (get-compliance-rule (rule-id uint))
  (map-get? compliance-rules { rule-id: rule-id })
)

(define-read-only (get-user-activity-pattern (user principal) (period uint))
  (map-get? user-activity-patterns { user: user, period: period })
)

(define-read-only (get-compliance-violation (user principal) (violation-id uint))
  (map-get? compliance-violations { user: user, violation-id: violation-id })
)

(define-read-only (calculate-risk-level (compliance-score uint))
  (if (<= compliance-score HIGH-RISK-THRESHOLD)
    "high"
    (if (<= compliance-score MEDIUM-RISK-THRESHOLD)
      "medium"
      "low")
  )
)

(define-read-only (get-system-compliance-stats)
  (ok {
    total-monitored-users: (var-get total-monitored-users),
    total-compliance-flags: (var-get total-compliance-flags),
    compliance-rules-version: (var-get compliance-rules-version),
    current-block: stacks-block-height
  })
)

;; Administrative functions

(define-public (create-compliance-rule (rule-id uint) (rule-name (string-ascii 50)) (violation-threshold uint) (penalty-score uint) (auto-flag bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (<= penalty-score u50) ERR-INVALID-PARAMS)
    (map-set compliance-rules
      { rule-id: rule-id }
      {
        rule-name: rule-name,
        violation-threshold: violation-threshold,
        penalty-score: penalty-score,
        auto-flag: auto-flag,
        active: true
      }
    )
    (ok rule-id)
  )
)

(define-public (update-kyc-status (user principal) (new-status (string-ascii 20)))
  (let ((profile (get-user-compliance-profile user)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set user-compliance-profiles
      { user: user }
      (merge profile {
        kyc-status: new-status,
        last-review: stacks-block-height
      })
    )
    (ok true)
  )
)

;; Core monitoring functions

(define-public (register-user-for-monitoring (user principal))
  (let ((existing-profile (map-get? user-compliance-profiles { user: user })))
    (asserts! (is-none existing-profile) ERR-ALREADY-EXISTS)
    (map-set user-compliance-profiles
      { user: user }
      {
        compliance-score: u100,
        risk-level: "low",
        total-violations: u0,
        last-review: stacks-block-height,
        kyc-status: "pending",
        monitoring-level: u1,
        flags-count: u0
      }
    )
    (var-set total-monitored-users (+ (var-get total-monitored-users) u1))
    (ok true)
  )
)

(define-public (record-activity-pattern (user principal) (transaction-count uint) (score-changes uint) (high-value-activities uint))
  (let (
    (current-period (/ stacks-block-height PATTERN-TRACKING-PERIOD))
    (existing-pattern (get-user-activity-pattern user current-period))
    (suspicious-count (calculate-suspicious-indicators transaction-count score-changes high-value-activities))
  )
    (map-set user-activity-patterns
      { user: user, period: current-period }
      {
        transaction-count: transaction-count,
        score-changes: score-changes,
        high-value-activities: high-value-activities,
        suspicious-indicators: suspicious-count,
        rapid-changes: (if (> score-changes u5) u1 u0)
      }
    )
    (if (> suspicious-count u3)
      (flag-suspicious-activity user current-period)
      (ok true)
    )
  )
)

(define-private (calculate-suspicious-indicators (tx-count uint) (score-changes uint) (high-value uint))
  (let (
    (tx-score (if (> tx-count u20) u2 (if (> tx-count u10) u1 u0)))
    (change-score (if (> score-changes u10) u2 (if (> score-changes u5) u1 u0)))
    (value-score (if (> high-value u5) u2 (if (> high-value u2) u1 u0)))
  )
    (+ tx-score change-score value-score)
  )
)

(define-private (flag-suspicious-activity (user principal) (period uint))
  (let (
    (profile (get-user-compliance-profile user))
    (violation-count (get-violation-count user))
    (new-violation-id (+ violation-count u1))
  )
    (map-set compliance-violations
      { user: user, violation-id: new-violation-id }
      {
        rule-violated: u1, ;; Suspicious activity rule
        severity: "medium",
        timestamp: stacks-block-height,
        resolved: false,
        description: "Suspicious activity pattern detected",
        auto-flagged: true
      }
    )
    (update-violation-count user new-violation-id)
    (update-compliance-score user u10) ;; Reduce score by 10
    (var-set total-compliance-flags (+ (var-get total-compliance-flags) u1))
    (ok true)
  )
)

(define-private (get-violation-count (user principal))
  (default-to u0 (get count (map-get? user-violation-counters { user: user })))
)

(define-private (update-violation-count (user principal) (new-count uint))
  (map-set user-violation-counters { user: user } { count: new-count })
)

(define-private (update-compliance-score (user principal) (penalty uint))
  (let ((profile (get-user-compliance-profile user)))
    (let ((new-score (if (>= (get compliance-score profile) penalty)
                       (- (get compliance-score profile) penalty)
                       u0)))
      (map-set user-compliance-profiles
        { user: user }
        (merge profile {
          compliance-score: new-score,
          risk-level: (calculate-risk-level new-score),
          total-violations: (+ (get total-violations profile) u1),
          flags-count: (+ (get flags-count profile) u1),
          last-review: stacks-block-height
        })
      )
    )
  )
)

(define-public (resolve-violation (user principal) (violation-id uint))
  (let ((violation (unwrap! (get-compliance-violation user violation-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (not (get resolved violation)) ERR-INVALID-PARAMS)
    (map-set compliance-violations
      { user: user, violation-id: violation-id }
      (merge violation { resolved: true })
    )
    (ok true)
  )
)

(define-public (generate-compliance-report (report-type (string-ascii 20)) (period-start uint) (period-end uint))
  (let (
    (report-id (+ (var-get report-counter) u1))
    (total-users (var-get total-monitored-users))
    (total-flags (var-get total-compliance-flags))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set compliance-reports
      { report-id: report-id }
      {
        report-type: report-type,
        period-start: period-start,
        period-end: period-end,
        total-users: total-users,
        flagged-users: (/ total-flags u3), ;; Estimate
        violations-count: total-flags,
        generated-by: tx-sender,
        timestamp: stacks-block-height
      }
    )
    (var-set report-counter report-id)
    (ok report-id)
  )
)

(define-read-only (get-compliance-report (report-id uint))
  (map-get? compliance-reports { report-id: report-id })
)

(define-public (perform-compliance-check (user principal))
  (let (
    (profile (get-user-compliance-profile user))
    (current-period (/ stacks-block-height PATTERN-TRACKING-PERIOD))
    (activity-pattern (get-user-activity-pattern user current-period))
  )
    (ok {
      user: user,
      compliance-score: (get compliance-score profile),
      risk-level: (get risk-level profile),
      kyc-status: (get kyc-status profile),
      requires-review: (< (get compliance-score profile) MEDIUM-RISK-THRESHOLD),
      activity-suspicious: (match activity-pattern
                             pattern (> (get suspicious-indicators pattern) u2)
                             false),
      last-review-blocks-ago: (- stacks-block-height (get last-review profile))
    })
  )
)
