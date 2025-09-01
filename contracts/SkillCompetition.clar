;; SkillCompetition - Dynamic Competition & Leaderboard System
;; Creates skill-based competitions with real-time leaderboards and performance tracking

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-not-found (err u201))
(define-constant err-unauthorized (err u202))
(define-constant err-invalid-amount (err u203))
(define-constant err-competition-exists (err u204))
(define-constant err-insufficient-balance (err u205))
(define-constant err-competition-ended (err u206))
(define-constant err-already-participating (err u207))
(define-constant err-not-participating (err u208))
(define-constant err-competition-not-ended (err u209))
(define-constant err-invalid-period (err u210))
(define-constant err-no-participants (err u211))
(define-constant err-reward-already-claimed (err u212))
(define-constant err-not-winner (err u213))
(define-constant err-competition-active (err u214))

(define-data-var next-competition-id uint u1)
(define-data-var next-submission-id uint u1)
(define-data-var platform-competition-fee uint u5)
(define-data-var leaderboard-update-interval uint u1440) ;; blocks per day
(define-data-var min-competition-duration uint u10080) ;; blocks per week

;; Competition definitions
(define-map competitions uint {
  category: (string-ascii 50),
  title: (string-ascii 200),
  description: (string-ascii 1000),
  creator: principal,
  start-block: uint,
  end-block: uint,
  entry-fee: uint,
  total-prize-pool: uint,
  prize-distribution: (list 10 uint), ;; percentage distribution for top 10
  participants-count: uint,
  min-participants: uint,
  max-participants: uint,
  evaluation-criteria: (string-ascii 500),
  status: (string-ascii 20), ;; active, ended, cancelled
  winners-announced: bool,
  total-submissions: uint
})

;; Participant data for competitions
(define-map competition-participants {competition-id: uint, participant: principal} {
  joined-at: uint,
  entry-fee-paid: uint,
  submissions-count: uint,
  total-score: uint,
  rank: uint,
  reward-claimed: bool,
  performance-metrics: {
    lesson-completion-rate: uint,
    average-rating: uint,
    total-lessons: uint,
    skill-validations: uint
  }
})

;; Competition submissions for performance tracking
(define-map competition-submissions uint {
  competition-id: uint,
  participant: principal,
  submission-type: (string-ascii 30), ;; lesson-completion, student-feedback, skill-validation
  submission-data: (string-ascii 500),
  score-achieved: uint,
  submitted-at: uint,
  verified: bool,
  verifier: (optional principal)
})

;; Leaderboards by category
(define-map category-leaderboards (string-ascii 50) {
  last-updated: uint,
  top-performers: (list 20 {teacher: principal, score: uint, rank: uint}),
  total-teachers: uint,
  competition-count: uint
})

;; Teacher performance history
(define-map teacher-performance principal {
  total-competitions: uint,
  competitions-won: uint,
  total-earnings: uint,
  best-rank: uint,
  performance-score: uint,
  specialty-categories: (list 5 (string-ascii 50)),
  streak-count: uint,
  last-competition: uint
})

;; Competition participant lists
(define-map competition-participant-list uint (list 100 principal))

;; Monthly/seasonal competition cycles
(define-map competition-cycles (string-ascii 50) {
  current-season: uint,
  season-start: uint,
  season-end: uint,
  active-competitions: (list 10 uint),
  completed-competitions: (list 50 uint),
  grand-champion: (optional principal),
  total-prize-distributed: uint
})

(define-public (create-competition
  (category (string-ascii 50))
  (title (string-ascii 200))
  (description (string-ascii 1000))
  (duration-blocks uint)
  (entry-fee uint)
  (initial-prize-pool uint)
  (min-participants uint)
  (max-participants uint)
  (evaluation-criteria (string-ascii 500)))
  (let (
    (competition-id (var-get next-competition-id))
    (current-block stacks-block-height)
    (end-block (+ current-block duration-blocks))
    (creator-balance (contract-call? .Skillsync get-user-balance tx-sender))
  )
    (asserts! (>= duration-blocks (var-get min-competition-duration)) err-invalid-period)
    (asserts! (> min-participants u0) err-invalid-amount)
    (asserts! (>= max-participants min-participants) err-invalid-amount)
    (asserts! (<= max-participants u100) err-invalid-amount)
    (asserts! (>= creator-balance initial-prize-pool) err-insufficient-balance)
    (asserts! (> entry-fee u0) err-invalid-amount)
    
    ;; Transfer initial prize pool from creator
    (try! (contract-call? .Skillsync deposit-funds initial-prize-pool))
    
    (map-set competitions competition-id {
      category: category,
      title: title,
      description: description,
      creator: tx-sender,
      start-block: current-block,
      end-block: end-block,
      entry-fee: entry-fee,
      total-prize-pool: initial-prize-pool,
      prize-distribution: (list u40 u25 u15 u10 u5 u3 u1 u1), ;; top 8 get rewards
      participants-count: u0,
      min-participants: min-participants,
      max-participants: max-participants,
      evaluation-criteria: evaluation-criteria,
      status: "active",
      winners-announced: false,
      total-submissions: u0
    })
    
    ;; Update category leaderboard
    (update-category-stats category)
    
    (var-set next-competition-id (+ competition-id u1))
    (ok competition-id)
  )
)

(define-public (join-competition (competition-id uint))
  (let (
    (competition-data (unwrap! (map-get? competitions competition-id) err-not-found))
    (participant-key {competition-id: competition-id, participant: tx-sender})
    (current-block stacks-block-height)
    (user-balance (contract-call? .Skillsync get-user-balance tx-sender))
    (entry-fee (get entry-fee competition-data))
    (current-participants (default-to (list) (map-get? competition-participant-list competition-id)))
  )
    (asserts! (is-eq (get status competition-data) "active") err-competition-ended)
    (asserts! (< current-block (get end-block competition-data)) err-competition-ended)
    (asserts! (< (get participants-count competition-data) (get max-participants competition-data)) err-invalid-amount)
    (asserts! (is-none (map-get? competition-participants participant-key)) err-already-participating)
    (asserts! (>= user-balance entry-fee) err-insufficient-balance)
    
    ;; Collect entry fee
    (try! (contract-call? .Skillsync withdraw-funds entry-fee))
    
    ;; Register participant
    (map-set competition-participants participant-key {
      joined-at: current-block,
      entry-fee-paid: entry-fee,
      submissions-count: u0,
      total-score: u0,
      rank: u0,
      reward-claimed: false,
      performance-metrics: {
        lesson-completion-rate: u0,
        average-rating: u0,
        total-lessons: u0,
        skill-validations: u0
      }
    })
    
    ;; Update participant list and competition data
    (map-set competition-participant-list competition-id 
      (unwrap! (as-max-len? (append current-participants tx-sender) u100) err-invalid-amount))
    
    (map-set competitions competition-id (merge competition-data {
      participants-count: (+ (get participants-count competition-data) u1),
      total-prize-pool: (+ (get total-prize-pool competition-data) entry-fee)
    }))
    
    (ok true)
  )
)

(define-public (submit-performance-data
  (competition-id uint)
  (submission-type (string-ascii 30))
  (submission-data (string-ascii 500))
  (score-achieved uint))
  (let (
    (submission-id (var-get next-submission-id))
    (competition-data (unwrap! (map-get? competitions competition-id) err-not-found))
    (participant-key {competition-id: competition-id, participant: tx-sender})
    (participant-data (unwrap! (map-get? competition-participants participant-key) err-not-participating))
    (current-block stacks-block-height)
  )
    (asserts! (is-eq (get status competition-data) "active") err-competition-ended)
    (asserts! (< current-block (get end-block competition-data)) err-competition-ended)
    (asserts! (<= score-achieved u100) err-invalid-amount)
    
    (map-set competition-submissions submission-id {
      competition-id: competition-id,
      participant: tx-sender,
      submission-type: submission-type,
      submission-data: submission-data,
      score-achieved: score-achieved,
      submitted-at: current-block,
      verified: false,
      verifier: none
    })
    
    ;; Update participant data
    (map-set competition-participants participant-key (merge participant-data {
      submissions-count: (+ (get submissions-count participant-data) u1),
      total-score: (+ (get total-score participant-data) score-achieved)
    }))
    
    ;; Update competition submission count
    (map-set competitions competition-id (merge competition-data {
      total-submissions: (+ (get total-submissions competition-data) u1)
    }))
    
    (var-set next-submission-id (+ submission-id u1))
    (ok submission-id)
  )
)

(define-public (end-competition-and-calculate-winners (competition-id uint))
  (let (
    (competition-data (unwrap! (map-get? competitions competition-id) err-not-found))
    (current-block stacks-block-height)
    (participants (default-to (list) (map-get? competition-participant-list competition-id)))
  )
    (asserts! (or (is-eq tx-sender (get creator competition-data)) (is-eq tx-sender contract-owner)) err-unauthorized)
    (asserts! (>= current-block (get end-block competition-data)) err-competition-not-ended)
    (asserts! (is-eq (get status competition-data) "active") err-competition-ended)
    (asserts! (>= (get participants-count competition-data) (get min-participants competition-data)) err-no-participants)
    
    ;; Mark competition as ended
    (map-set competitions competition-id (merge competition-data {
      status: "ended",
      winners-announced: true
    }))
    
    ;; Calculate and assign ranks (simplified - would need complex sorting in production)
    (calculate-participant-ranks competition-id participants)
    
    (ok true)
  )
)

(define-private (calculate-participant-ranks (competition-id uint) (participants (list 100 principal)))
  (let ((competition-data (unwrap-panic (map-get? competitions competition-id))))
    ;; Simplified ranking - in production would need proper sorting algorithm
    (map calculate-single-rank participants)
    true
  )
)

(define-private (calculate-single-rank (participant principal))
  ;; Simplified rank calculation - would implement proper scoring logic
  true
)

(define-public (claim-competition-reward (competition-id uint))
  (let (
    (competition-data (unwrap! (map-get? competitions competition-id) err-not-found))
    (participant-key {competition-id: competition-id, participant: tx-sender})
    (participant-data (unwrap! (map-get? competition-participants participant-key) err-not-participating))
    (participant-rank (get rank participant-data))
    (total-prize (get total-prize-pool competition-data))
    (prize-distribution (get prize-distribution competition-data))
  )
    (asserts! (is-eq (get status competition-data) "ended") err-competition-not-ended)
    (asserts! (get winners-announced competition-data) err-not-winner)
    (asserts! (not (get reward-claimed participant-data)) err-reward-already-claimed)
    (asserts! (> participant-rank u0) err-not-winner)
    (asserts! (<= participant-rank u10) err-not-winner)
    
    (let (
      (reward-percentage (unwrap! (element-at prize-distribution (- participant-rank u1)) err-not-winner))
      (reward-amount (/ (* total-prize reward-percentage) u100))
      (platform-fee (/ (* reward-amount (var-get platform-competition-fee)) u100))
      (net-reward (- reward-amount platform-fee))
    )
      ;; Transfer reward to winner
      (try! (contract-call? .Skillsync deposit-funds net-reward))
      
      ;; Update participant data
      (map-set competition-participants participant-key (merge participant-data {
        reward-claimed: true
      }))
      
      ;; Update teacher performance history
      (update-teacher-performance-history tx-sender competition-id participant-rank net-reward)
      
      (ok net-reward)
    )
  )
)

(define-private (update-teacher-performance-history 
  (teacher principal) 
  (competition-id uint) 
  (rank uint) 
  (earnings uint))
  (let (
    (current-performance (default-to {
      total-competitions: u0,
      competitions-won: u0,
      total-earnings: u0,
      best-rank: u999,
      performance-score: u0,
      specialty-categories: (list),
      streak-count: u0,
      last-competition: u0
    } (map-get? teacher-performance teacher)))
    (competitions-won (if (is-eq rank u1) (+ (get competitions-won current-performance) u1) (get competitions-won current-performance)))
    (best-rank (if (< rank (get best-rank current-performance)) rank (get best-rank current-performance)))
  )
    (map-set teacher-performance teacher (merge current-performance {
      total-competitions: (+ (get total-competitions current-performance) u1),
      competitions-won: competitions-won,
      total-earnings: (+ (get total-earnings current-performance) earnings),
      best-rank: best-rank,
      last-competition: competition-id
    }))
    true
  )
)

(define-private (update-category-stats (category (string-ascii 50)))
  (let (
    (current-leaderboard (default-to {
      last-updated: u0,
      top-performers: (list),
      total-teachers: u0,
      competition-count: u0
    } (map-get? category-leaderboards category)))
  )
    (map-set category-leaderboards category (merge current-leaderboard {
      last-updated: stacks-block-height,
      competition-count: (+ (get competition-count current-leaderboard) u1)
    }))
    true
  )
)

(define-public (update-leaderboard-rankings (category (string-ascii 50)))
  (let (
    (current-leaderboard (default-to {
      last-updated: u0,
      top-performers: (list),
      total-teachers: u0,
      competition-count: u0
    } (map-get? category-leaderboards category)))
    (current-block stacks-block-height)
    (last-update (get last-updated current-leaderboard))
  )
    (asserts! (>= (- current-block last-update) (var-get leaderboard-update-interval)) err-invalid-period)
    
    ;; Update leaderboard with fresh data
    (map-set category-leaderboards category (merge current-leaderboard {
      last-updated: current-block
    }))
    
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-competition (competition-id uint))
  (map-get? competitions competition-id)
)

(define-read-only (get-competition-participant (competition-id uint) (participant principal))
  (map-get? competition-participants {competition-id: competition-id, participant: participant})
)

(define-read-only (get-competition-submission (submission-id uint))
  (map-get? competition-submissions submission-id)
)

(define-read-only (get-category-leaderboard (category (string-ascii 50)))
  (map-get? category-leaderboards category)
)

(define-read-only (get-teacher-performance (teacher principal))
  (map-get? teacher-performance teacher)
)

(define-read-only (get-competition-participants (competition-id uint))
  (default-to (list) (map-get? competition-participant-list competition-id))
)

(define-read-only (get-active-competitions-by-category (category (string-ascii 50)))
  (match (map-get? competition-cycles category)
    cycle-data (get active-competitions cycle-data)
    (list))
)

(define-read-only (get-next-competition-id)
  (var-get next-competition-id)
)

(define-read-only (get-next-submission-id)
  (var-get next-submission-id)
)

(define-read-only (get-platform-competition-fee)
  (var-get platform-competition-fee)
)

;; Admin functions
(define-public (update-platform-competition-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u20) err-invalid-amount)
    (var-set platform-competition-fee new-fee)
    (ok true)
  )
)

(define-public (update-leaderboard-interval (new-interval uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (>= new-interval u144) err-invalid-period) ;; minimum 1 day
    (var-set leaderboard-update-interval new-interval)
    (ok true)
  )
)

(define-public (emergency-cancel-competition (competition-id uint))
  (let ((competition-data (unwrap! (map-get? competitions competition-id) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (is-eq (get status competition-data) "ended")) err-competition-ended)
    (map-set competitions competition-id (merge competition-data {status: "cancelled"}))
    (ok true)
  )
)
