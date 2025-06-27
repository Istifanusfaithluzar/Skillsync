;; Skillsync - Skills Transfer DAO
;; Immigrants validate & teach homeland skills

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-skill-exists (err u104))
(define-constant err-insufficient-balance (err u105))
(define-constant err-lesson-not-pending (err u106))
(define-constant err-already-validated (err u107))

(define-data-var next-skill-id uint u1)
(define-data-var next-lesson-id uint u1)
(define-data-var platform-fee uint u5)
(define-data-var min-validation-stake uint u1000000)

(define-map teachers principal {
  name: (string-ascii 100),
  bio: (string-ascii 500),
  reputation: uint,
  total-earnings: uint,
  skills-count: uint,
  joined-at: uint
})

(define-map skills uint {
  teacher: principal,
  title: (string-ascii 200),
  description: (string-ascii 1000),
  category: (string-ascii 50),
  price-per-hour: uint,
  validations: uint,
  total-lessons: uint,
  rating: uint,
  created-at: uint,
  active: bool
})

(define-map lessons uint {
  skill-id: uint,
  student: principal,
  teacher: principal,
  duration-hours: uint,
  total-cost: uint,
  status: (string-ascii 20),
  scheduled-at: uint,
  completed-at: (optional uint),
  student-rating: (optional uint),
  teacher-rating: (optional uint)
})

(define-map skill-validations {skill-id: uint, validator: principal} {
  stake: uint,
  validated-at: uint,
  is-valid: bool
})

(define-map user-balances principal uint)

(define-map teacher-skills principal (list 100 uint))

(define-public (register-teacher (name (string-ascii 100)) (bio (string-ascii 500)))
  (let ((current-block stacks-block-height))
    (asserts! (is-none (map-get? teachers tx-sender)) err-skill-exists)
    (map-set teachers tx-sender {
      name: name,
      bio: bio,
      reputation: u0,
      total-earnings: u0,
      skills-count: u0,
      joined-at: current-block
    })
    (ok true)
  )
)

(define-public (add-skill 
  (title (string-ascii 200))
  (description (string-ascii 1000))
  (category (string-ascii 50))
  (price-per-hour uint))
  (let (
    (skill-id (var-get next-skill-id))
    (current-block stacks-block-height)
    (teacher-data (unwrap! (map-get? teachers tx-sender) err-not-found))
    (current-skills (default-to (list) (map-get? teacher-skills tx-sender)))
  )
    (asserts! (> price-per-hour u0) err-invalid-amount)
    (map-set skills skill-id {
      teacher: tx-sender,
      title: title,
      description: description,
      category: category,
      price-per-hour: price-per-hour,
      validations: u0,
      total-lessons: u0,
      rating: u0,
      created-at: current-block,
      active: true
    })
    (map-set teacher-skills tx-sender (unwrap! (as-max-len? (append current-skills skill-id) u100) err-invalid-amount))
    (map-set teachers tx-sender (merge teacher-data {skills-count: (+ (get skills-count teacher-data) u1)}))
    (var-set next-skill-id (+ skill-id u1))
    (ok skill-id)
  )
)

(define-public (validate-skill (skill-id uint))
  (let (
    (skill-data (unwrap! (map-get? skills skill-id) err-not-found))
    (validation-key {skill-id: skill-id, validator: tx-sender})
    (stake-amount (var-get min-validation-stake))
    (current-balance (default-to u0 (map-get? user-balances tx-sender)))
    (current-block stacks-block-height)
  )
    (asserts! (>= current-balance stake-amount) err-insufficient-balance)
    (asserts! (is-none (map-get? skill-validations validation-key)) err-already-validated)
    (asserts! (not (is-eq tx-sender (get teacher skill-data))) err-unauthorized)
    (map-set user-balances tx-sender (- current-balance stake-amount))
    (map-set skill-validations validation-key {
      stake: stake-amount,
      validated-at: current-block,
      is-valid: true
    })
    (map-set skills skill-id (merge skill-data {validations: (+ (get validations skill-data) u1)}))
    (ok true)
  )
)

(define-public (book-lesson (skill-id uint) (duration-hours uint) (scheduled-at uint))
  (let (
    (lesson-id (var-get next-lesson-id))
    (skill-data (unwrap! (map-get? skills skill-id) err-not-found))
    (total-cost (* (get price-per-hour skill-data) duration-hours))
    (platform-cost (/ (* total-cost (var-get platform-fee)) u100))
    (teacher-cost (- total-cost platform-cost))
    (current-balance (default-to u0 (map-get? user-balances tx-sender)))
  )
    (asserts! (get active skill-data) err-unauthorized)
    (asserts! (> duration-hours u0) err-invalid-amount)
    (asserts! (>= current-balance total-cost) err-insufficient-balance)
    (asserts! (not (is-eq tx-sender (get teacher skill-data))) err-unauthorized)
    (map-set user-balances tx-sender (- current-balance total-cost))
    (map-set lessons lesson-id {
      skill-id: skill-id,
      student: tx-sender,
      teacher: (get teacher skill-data),
      duration-hours: duration-hours,
      total-cost: total-cost,
      status: "pending",
      scheduled-at: scheduled-at,
      completed-at: none,
      student-rating: none,
      teacher-rating: none
    })
    (var-set next-lesson-id (+ lesson-id u1))
    (ok lesson-id)
  )
)

(define-public (complete-lesson (lesson-id uint))
  (let (
    (lesson-data (unwrap! (map-get? lessons lesson-id) err-not-found))
    (current-block stacks-block-height)
    (skill-data (unwrap! (map-get? skills (get skill-id lesson-data)) err-not-found))
    (teacher-data (unwrap! (map-get? teachers (get teacher lesson-data)) err-not-found))
    (platform-cost (/ (* (get total-cost lesson-data) (var-get platform-fee)) u100))
    (teacher-earnings (- (get total-cost lesson-data) platform-cost))
    (teacher-balance (default-to u0 (map-get? user-balances (get teacher lesson-data))))
  )
    (asserts! (is-eq tx-sender (get teacher lesson-data)) err-unauthorized)
    (asserts! (is-eq (get status lesson-data) "pending") err-lesson-not-pending)
    (map-set lessons lesson-id (merge lesson-data {
      status: "completed",
      completed-at: (some current-block)
    }))
    (map-set skills (get skill-id lesson-data) (merge skill-data {
      total-lessons: (+ (get total-lessons skill-data) u1)
    }))
    (map-set teachers (get teacher lesson-data) (merge teacher-data {
      total-earnings: (+ (get total-earnings teacher-data) teacher-earnings)
    }))
    (map-set user-balances (get teacher lesson-data) (+ teacher-balance teacher-earnings))
    (ok true)
  )
)

(define-public (rate-lesson (lesson-id uint) (rating uint) (is-student bool))
  (let ((lesson-data (unwrap! (map-get? lessons lesson-id) err-not-found)))
    (asserts! (<= rating u5) err-invalid-amount)
    (asserts! (>= rating u1) err-invalid-amount)
    (asserts! (is-eq (get status lesson-data) "completed") err-lesson-not-pending)
    (if is-student
      (begin
        (asserts! (is-eq tx-sender (get student lesson-data)) err-unauthorized)
        (map-set lessons lesson-id (merge lesson-data {student-rating: (some rating)}))
      )
      (begin
        (asserts! (is-eq tx-sender (get teacher lesson-data)) err-unauthorized)
        (map-set lessons lesson-id (merge lesson-data {teacher-rating: (some rating)}))
      )
    )
    (ok true)
  )
)

(define-public (deposit-funds (amount uint))
  (let ((current-balance (default-to u0 (map-get? user-balances tx-sender))))
    (asserts! (> amount u0) err-invalid-amount)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set user-balances tx-sender (+ current-balance amount))
    (ok true)
  )
)

(define-public (withdraw-funds (amount uint))
  (let ((current-balance (default-to u0 (map-get? user-balances tx-sender))))
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= current-balance amount) err-insufficient-balance)
    (map-set user-balances tx-sender (- current-balance amount))
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (ok true)
  )
)

(define-public (toggle-skill-status (skill-id uint))
  (let ((skill-data (unwrap! (map-get? skills skill-id) err-not-found)))
    (asserts! (is-eq tx-sender (get teacher skill-data)) err-unauthorized)
    (map-set skills skill-id (merge skill-data {active: (not (get active skill-data))}))
    (ok true)
  )
)

(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u20) err-invalid-amount)
    (var-set platform-fee new-fee)
    (ok true)
  )
)

(define-read-only (get-teacher (teacher principal))
  (map-get? teachers teacher)
)

(define-read-only (get-skill (skill-id uint))
  (map-get? skills skill-id)
)

(define-read-only (get-lesson (lesson-id uint))
  (map-get? lessons lesson-id)
)

(define-read-only (get-user-balance (user principal))
  (default-to u0 (map-get? user-balances user))
)

(define-read-only (get-teacher-skills (teacher principal))
  (default-to (list) (map-get? teacher-skills teacher))
)

(define-read-only (get-skill-validation (skill-id uint) (validator principal))
  (map-get? skill-validations {skill-id: skill-id, validator: validator})
)

(define-read-only (get-platform-fee)
  (var-get platform-fee)
)

(define-read-only (get-next-skill-id)
  (var-get next-skill-id)
)

(define-read-only (get-next-lesson-id)
  (var-get next-lesson-id)
)
