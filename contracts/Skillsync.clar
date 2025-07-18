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
(define-constant err-not-authority (err u108))
(define-constant err-already-certified (err u109))
(define-constant err-certificate-not-found (err u110))
(define-constant err-authority-exists (err u111))
(define-constant err-invalid-price (err u112))
(define-constant err-certificate-expired (err u113))

(define-data-var next-skill-id uint u1)
(define-data-var next-lesson-id uint u1)
(define-data-var next-certificate-id uint u1)
(define-data-var platform-fee uint u5)
(define-data-var min-validation-stake uint u1000000)
(define-data-var certificate-fee uint u500000)

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

(define-map certification-authorities principal {
  name: (string-ascii 100),
  description: (string-ascii 500),
  website: (string-ascii 200),
  active: bool,
  total-certificates: uint,
  created-at: uint,
  verification-fee: uint
})

(define-map skill-certificates uint {
  skill-id: uint,
  teacher: principal,
  authority: principal,
  certificate-name: (string-ascii 200),
  certificate-description: (string-ascii 500),
  issue-date: uint,
  expiry-date: uint,
  certificate-level: (string-ascii 50),
  verification-code: (string-ascii 100),
  active: bool
})

(define-map authority-certificates principal (list 200 uint))

(define-map teacher-certificates principal (list 50 uint))

(define-map certificate-marketplace uint {
  certificate-id: uint,
  seller: principal,
  price: uint,
  for-sale: bool,
  created-at: uint
})

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

(define-public (register-certification-authority 
  (name (string-ascii 100))
  (description (string-ascii 500))
  (website (string-ascii 200))
  (verification-fee uint))
  (let ((current-block stacks-block-height))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-none (map-get? certification-authorities tx-sender)) err-authority-exists)
    (asserts! (> verification-fee u0) err-invalid-price)
    (map-set certification-authorities tx-sender {
      name: name,
      description: description,
      website: website,
      active: true,
      total-certificates: u0,
      created-at: current-block,
      verification-fee: verification-fee
    })
    (ok true)
  )
)

(define-public (update-authority-status (authority principal) (active bool))
  (let ((authority-data (unwrap! (map-get? certification-authorities authority) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set certification-authorities authority (merge authority-data {active: active}))
    (ok true)
  )
)

(define-public (issue-skill-certificate
  (skill-id uint)
  (teacher principal)
  (certificate-name (string-ascii 200))
  (certificate-description (string-ascii 500))
  (certificate-level (string-ascii 50))
  (verification-code (string-ascii 100))
  (validity-days uint))
  (let (
    (certificate-id (var-get next-certificate-id))
    (current-block stacks-block-height)
    (expiry-date (+ current-block validity-days))
    (skill-data (unwrap! (map-get? skills skill-id) err-not-found))
    (authority-data (unwrap! (map-get? certification-authorities tx-sender) err-not-authority))
    (teacher-certs (default-to (list) (map-get? teacher-certificates teacher)))
    (authority-certs (default-to (list) (map-get? authority-certificates tx-sender)))
    (verification-fee (get verification-fee authority-data))
    (teacher-balance (default-to u0 (map-get? user-balances teacher)))
  )
    (asserts! (get active authority-data) err-unauthorized)
    (asserts! (is-eq teacher (get teacher skill-data)) err-unauthorized)
    (asserts! (>= teacher-balance verification-fee) err-insufficient-balance)
    (asserts! (> validity-days u0) err-invalid-amount)
    (map-set user-balances teacher (- teacher-balance verification-fee))
    (map-set user-balances tx-sender (+ (default-to u0 (map-get? user-balances tx-sender)) verification-fee))
    (map-set skill-certificates certificate-id {
      skill-id: skill-id,
      teacher: teacher,
      authority: tx-sender,
      certificate-name: certificate-name,
      certificate-description: certificate-description,
      issue-date: current-block,
      expiry-date: expiry-date,
      certificate-level: certificate-level,
      verification-code: verification-code,
      active: true
    })
    (map-set teacher-certificates teacher (unwrap! (as-max-len? (append teacher-certs certificate-id) u50) err-invalid-amount))
    (map-set authority-certificates tx-sender (unwrap! (as-max-len? (append authority-certs certificate-id) u200) err-invalid-amount))
    (map-set certification-authorities tx-sender (merge authority-data {
      total-certificates: (+ (get total-certificates authority-data) u1)
    }))
    (var-set next-certificate-id (+ certificate-id u1))
    (ok certificate-id)
  )
)

(define-public (revoke-certificate (certificate-id uint))
  (let ((certificate-data (unwrap! (map-get? skill-certificates certificate-id) err-certificate-not-found)))
    (asserts! (is-eq tx-sender (get authority certificate-data)) err-unauthorized)
    (map-set skill-certificates certificate-id (merge certificate-data {active: false}))
    (ok true)
  )
)

(define-public (verify-certificate (certificate-id uint) (verification-code (string-ascii 100)))
  (let ((certificate-data (unwrap! (map-get? skill-certificates certificate-id) err-certificate-not-found)))
    (asserts! (is-eq verification-code (get verification-code certificate-data)) err-unauthorized)
    (asserts! (get active certificate-data) err-certificate-expired)
    (asserts! (> (get expiry-date certificate-data) stacks-block-height) err-certificate-expired)
    (ok certificate-data)
  )
)

(define-public (list-certificate-for-sale (certificate-id uint) (price uint))
  (let (
    (certificate-data (unwrap! (map-get? skill-certificates certificate-id) err-certificate-not-found))
    (current-block stacks-block-height)
  )
    (asserts! (is-eq tx-sender (get teacher certificate-data)) err-unauthorized)
    (asserts! (get active certificate-data) err-certificate-expired)
    (asserts! (> price u0) err-invalid-price)
    (map-set certificate-marketplace certificate-id {
      certificate-id: certificate-id,
      seller: tx-sender,
      price: price,
      for-sale: true,
      created-at: current-block
    })
    (ok true)
  )
)

(define-public (buy-certificate (certificate-id uint))
  (let (
    (marketplace-data (unwrap! (map-get? certificate-marketplace certificate-id) err-not-found))
    (certificate-data (unwrap! (map-get? skill-certificates certificate-id) err-certificate-not-found))
    (buyer-balance (default-to u0 (map-get? user-balances tx-sender)))
    (seller-balance (default-to u0 (map-get? user-balances (get seller marketplace-data))))
    (price (get price marketplace-data))
    (platform-cost (/ (* price (var-get platform-fee)) u100))
    (seller-earnings (- price platform-cost))
    (buyer-certs (default-to (list) (map-get? teacher-certificates tx-sender)))
  )
    (asserts! (get for-sale marketplace-data) err-unauthorized)
    (asserts! (get active certificate-data) err-certificate-expired)
    (asserts! (>= buyer-balance price) err-insufficient-balance)
    (asserts! (not (is-eq tx-sender (get seller marketplace-data))) err-unauthorized)
    (map-set user-balances tx-sender (- buyer-balance price))
    (map-set user-balances (get seller marketplace-data) (+ seller-balance seller-earnings))
    (map-set skill-certificates certificate-id (merge certificate-data {teacher: tx-sender}))
    (map-set teacher-certificates tx-sender (unwrap! (as-max-len? (append buyer-certs certificate-id) u50) err-invalid-amount))
    (map-set certificate-marketplace certificate-id (merge marketplace-data {for-sale: false}))
    (ok true)
  )
)

(define-public (remove-certificate-from-sale (certificate-id uint))
  (let ((marketplace-data (unwrap! (map-get? certificate-marketplace certificate-id) err-not-found)))
    (asserts! (is-eq tx-sender (get seller marketplace-data)) err-unauthorized)
    (map-set certificate-marketplace certificate-id (merge marketplace-data {for-sale: false}))
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

(define-read-only (get-certification-authority (authority principal))
  (map-get? certification-authorities authority)
)

(define-read-only (get-skill-certificate (certificate-id uint))
  (map-get? skill-certificates certificate-id)
)

(define-read-only (get-teacher-certificates (teacher principal))
  (default-to (list) (map-get? teacher-certificates teacher))
)

(define-read-only (get-authority-certificates (authority principal))
  (default-to (list) (map-get? authority-certificates authority))
)

(define-read-only (get-certificate-marketplace-info (certificate-id uint))
  (map-get? certificate-marketplace certificate-id)
)

(define-read-only (get-next-certificate-id)
  (var-get next-certificate-id)
)

(define-read-only (get-certificate-fee)
  (var-get certificate-fee)
)

(define-read-only (is-certificate-valid (certificate-id uint))
  (match (map-get? skill-certificates certificate-id)
    certificate-data (and 
      (get active certificate-data) 
      (> (get expiry-date certificate-data) stacks-block-height))
    false)
)
