;; SkillEndorsement - Peer-to-peer skill endorsement system
;; Teachers can endorse each other's skills building trust and credibility

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-not-found (err u301))
(define-constant err-unauthorized (err u302))
(define-constant err-invalid-amount (err u303))
(define-constant err-self-endorsement (err u304))
(define-constant err-already-endorsed (err u305))
(define-constant err-insufficient-reputation (err u306))
(define-constant err-endorsement-limit (err u307))
(define-constant err-skill-not-active (err u308))
(define-constant err-teacher-not-found (err u309))

(define-data-var next-endorsement-id uint u1)
(define-data-var min-endorser-reputation uint u10)
(define-data-var max-daily-endorsements uint u5)
(define-data-var endorsement-weight-multiplier uint u100)

;; Endorsement records
(define-map endorsements uint {
  endorser: principal,
  endorsed-teacher: principal,
  skill-id: uint,
  endorsement-text: (string-ascii 300),
  credibility-score: uint,
  created-at: uint,
  verified: bool,
  weight: uint
})

;; Track endorsements between teachers for specific skills
(define-map skill-endorsements {skill-id: uint, endorser: principal, endorsed: principal} {
  endorsement-id: uint,
  strength: uint, ;; 1-5 scale
  expertise-level: (string-ascii 20), ;; beginner, intermediate, advanced, expert
  created-at: uint
})

;; Aggregated endorsement data per skill
(define-map skill-endorsement-stats uint {
  total-endorsements: uint,
  average-strength: uint,
  expert-endorsements: uint,
  last-endorsement: uint,
  top-endorsers: (list 5 principal),
  credibility-score: uint
})

;; Teacher endorsement activity tracking
(define-map teacher-endorsement-activity principal {
  endorsements-given: uint,
  endorsements-received: uint,
  daily-endorsements-count: uint,
  last-endorsement-date: uint,
  reputation-from-endorsements: uint,
  trusted-endorser-status: bool
})

;; Daily endorsement limits per teacher
(define-map daily-endorsement-count {teacher: principal, day: uint} uint)

;; Teacher endorsement lists
(define-map teacher-given-endorsements principal (list 50 uint))
(define-map teacher-received-endorsements principal (list 100 uint))
(define-map skill-endorsement-list uint (list 20 uint))

;; Cross-skill endorsement patterns
(define-map endorsement-networks principal {
  frequent-endorsers: (list 10 principal),
  endorsement-categories: (list 5 (string-ascii 50)),
  network-strength: uint,
  mutual-endorsements: uint
})

(define-public (endorse-skill
  (teacher principal)
  (skill-id uint)
  (strength uint)
  (expertise-level (string-ascii 20))
  (endorsement-text (string-ascii 300)))
  (let (
    (endorsement-id (var-get next-endorsement-id))
    (current-block stacks-block-height)
    (current-day (/ current-block u144)) ;; assuming 144 blocks per day
    (endorser-teacher (unwrap! (contract-call? .Skillsync get-teacher tx-sender) err-teacher-not-found))
    (endorsed-teacher-data (unwrap! (contract-call? .Skillsync get-teacher teacher) err-teacher-not-found))
    (skill-data (unwrap! (contract-call? .Skillsync get-skill skill-id) err-not-found))
    (endorser-reputation (get reputation endorser-teacher))
    (endorsement-key {skill-id: skill-id, endorser: tx-sender, endorsed: teacher})
    (daily-count (default-to u0 (map-get? daily-endorsement-count {teacher: tx-sender, day: current-day})))
    (endorser-activity (default-to {
      endorsements-given: u0,
      endorsements-received: u0,
      daily-endorsements-count: u0,
      last-endorsement-date: u0,
      reputation-from-endorsements: u0,
      trusted-endorser-status: false
    } (map-get? teacher-endorsement-activity tx-sender)))
  )
    ;; Validation checks
    (asserts! (not (is-eq tx-sender teacher)) err-self-endorsement)
    (asserts! (is-eq teacher (get teacher skill-data)) err-unauthorized)
    (asserts! (get active skill-data) err-skill-not-active)
    (asserts! (>= endorser-reputation (var-get min-endorser-reputation)) err-insufficient-reputation)
    (asserts! (< daily-count (var-get max-daily-endorsements)) err-endorsement-limit)
    (asserts! (is-none (map-get? skill-endorsements endorsement-key)) err-already-endorsed)
    (asserts! (and (>= strength u1) (<= strength u5)) err-invalid-amount)
    
    ;; Calculate endorsement weight based on endorser's reputation and experience
    (let (
      (weight (calculate-endorsement-weight endorser-reputation (get total-earnings endorsed-teacher-data)))
      (given-endorsements (default-to (list) (map-get? teacher-given-endorsements tx-sender)))
      (received-endorsements (default-to (list) (map-get? teacher-received-endorsements teacher)))
      (skill-stats (default-to {
        total-endorsements: u0,
        average-strength: u0,
        expert-endorsements: u0,
        last-endorsement: u0,
        top-endorsers: (list),
        credibility-score: u0
      } (map-get? skill-endorsement-stats skill-id)))
    )
      ;; Create endorsement record
      (map-set endorsements endorsement-id {
        endorser: tx-sender,
        endorsed-teacher: teacher,
        skill-id: skill-id,
        endorsement-text: endorsement-text,
        credibility-score: weight,
        created-at: current-block,
        verified: (>= endorser-reputation u50), ;; auto-verify high reputation teachers
        weight: weight
      })
      
      ;; Track skill-specific endorsement
      (map-set skill-endorsements endorsement-key {
        endorsement-id: endorsement-id,
        strength: strength,
        expertise-level: expertise-level,
        created-at: current-block
      })
      
      ;; Update skill endorsement statistics
      (update-skill-endorsement-stats skill-id strength weight)
      
      ;; Update teacher activity tracking
      (map-set teacher-endorsement-activity tx-sender (merge endorser-activity {
        endorsements-given: (+ (get endorsements-given endorser-activity) u1),
        daily-endorsements-count: (+ daily-count u1),
        last-endorsement-date: current-block
      }))
      
      ;; Update daily endorsement count
      (map-set daily-endorsement-count {teacher: tx-sender, day: current-day} (+ daily-count u1))
      
      ;; Update endorsement lists
      (map-set teacher-given-endorsements tx-sender 
        (unwrap! (as-max-len? (append given-endorsements endorsement-id) u50) err-endorsement-limit))
      (map-set teacher-received-endorsements teacher
        (unwrap! (as-max-len? (append received-endorsements endorsement-id) u100) err-endorsement-limit))
      
      (var-set next-endorsement-id (+ endorsement-id u1))
      (ok endorsement-id)
    )
  )
)

(define-private (calculate-endorsement-weight (endorser-reputation uint) (endorsed-lessons uint))
  (let (
    (base-weight (var-get endorsement-weight-multiplier))
    (reputation-multiplier (if (>= endorser-reputation u100) u3 
                          (if (>= endorser-reputation u50) u2 u1)))
    (experience-bonus (if (>= endorsed-lessons u20) u50 
                     (if (>= endorsed-lessons u10) u25 u0)))
  )
    (+ (* base-weight reputation-multiplier) experience-bonus)
  )
)

(define-private (update-skill-endorsement-stats (skill-id uint) (strength uint) (weight uint))
  (let (
    (current-stats (default-to {
      total-endorsements: u0,
      average-strength: u0,
      expert-endorsements: u0,
      last-endorsement: u0,
      top-endorsers: (list),
      credibility-score: u0
    } (map-get? skill-endorsement-stats skill-id)))
    (new-total (+ (get total-endorsements current-stats) u1))
    (new-average (/ (+ (* (get average-strength current-stats) (get total-endorsements current-stats)) strength) new-total))
    (new-expert-count (if (>= strength u4) (+ (get expert-endorsements current-stats) u1) (get expert-endorsements current-stats)))
    (new-credibility (+ (get credibility-score current-stats) weight))
  )
    (map-set skill-endorsement-stats skill-id (merge current-stats {
      total-endorsements: new-total,
      average-strength: new-average,
      expert-endorsements: new-expert-count,
      last-endorsement: stacks-block-height,
      credibility-score: new-credibility
    }))
    true
  )
)

(define-public (verify-endorsement (endorsement-id uint) (verified bool))
  (let ((endorsement-data (unwrap! (map-get? endorsements endorsement-id) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set endorsements endorsement-id (merge endorsement-data {verified: verified}))
    (ok true)
  )
)

(define-public (update-trusted-endorser-status (teacher principal) (trusted bool))
  (let (
    (activity-data (default-to {
      endorsements-given: u0,
      endorsements-received: u0,
      daily-endorsements-count: u0,
      last-endorsement-date: u0,
      reputation-from-endorsements: u0,
      trusted-endorser-status: false
    } (map-get? teacher-endorsement-activity teacher)))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set teacher-endorsement-activity teacher (merge activity-data {trusted-endorser-status: trusted}))
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-endorsement (endorsement-id uint))
  (map-get? endorsements endorsement-id)
)

(define-read-only (get-skill-endorsement (skill-id uint) (endorser principal) (endorsed principal))
  (map-get? skill-endorsements {skill-id: skill-id, endorser: endorser, endorsed: endorsed})
)

(define-read-only (get-skill-endorsement-stats (skill-id uint))
  (map-get? skill-endorsement-stats skill-id)
)

(define-read-only (get-teacher-endorsement-activity (teacher principal))
  (map-get? teacher-endorsement-activity teacher)
)

(define-read-only (get-teacher-given-endorsements (teacher principal))
  (default-to (list) (map-get? teacher-given-endorsements teacher))
)

(define-read-only (get-teacher-received-endorsements (teacher principal))
  (default-to (list) (map-get? teacher-received-endorsements teacher))
)

(define-read-only (get-daily-endorsement-count (teacher principal))
  (let ((current-day (/ stacks-block-height u144)))
    (default-to u0 (map-get? daily-endorsement-count {teacher: teacher, day: current-day}))
  )
)

(define-read-only (calculate-skill-trust-score (skill-id uint))
  (match (map-get? skill-endorsement-stats skill-id)
    stats-data (let (
      (endorsement-count (get total-endorsements stats-data))
      (avg-strength (get average-strength stats-data))
      (expert-count (get expert-endorsements stats-data))
      (credibility (get credibility-score stats-data))
    )
      (+
        (* endorsement-count u10) ;; base points per endorsement
        (* avg-strength u5)       ;; strength multiplier
        (* expert-count u20)      ;; expert endorsement bonus
        (/ credibility u10)       ;; credibility score contribution
      )
    )
    u0
  )
)

(define-read-only (get-endorsement-network (teacher principal))
  (map-get? endorsement-networks teacher)
)

(define-read-only (get-next-endorsement-id)
  (var-get next-endorsement-id)
)

;; Admin functions
(define-public (update-min-endorser-reputation (new-min uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set min-endorser-reputation new-min)
    (ok true)
  )
)

(define-public (update-max-daily-endorsements (new-max uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> new-max u0) err-invalid-amount)
    (var-set max-daily-endorsements new-max)
    (ok true)
  )
)
