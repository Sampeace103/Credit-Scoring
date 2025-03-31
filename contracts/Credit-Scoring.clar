;; title: Credit-Scoring
;; version: 1.0
;; summary: On-Chain Credit Scoring System
;; description: A system that tracks users' repayment history and assigns credit scores

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-LOAN-NOT-FOUND (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-INVALID-DURATION (err u103))
(define-constant ERR-LOAN-ALREADY-EXISTS (err u104))
(define-constant ERR-LOAN-NOT-ACTIVE (err u105))
(define-constant ERR-REPAYMENT-EXCEEDS-DEBT (err u106))
(define-constant ERR-INVALID-SCORE-PARAMS (err u107))

;; Credit score tiers
(define-constant SCORE-EXCELLENT u800) ;; 800+
(define-constant SCORE-GOOD u700)      ;; 700-799
(define-constant SCORE-FAIR u600)      ;; 600-699
(define-constant SCORE-POOR u500)      ;; 500-599
(define-constant SCORE-BAD u0)         ;; 0-499

;; Initial score for new users
(define-constant INITIAL-SCORE u650)

;; Maximum score possible
(define-constant MAX-SCORE u850)

;; Data vars
(define-data-var admin principal CONTRACT-OWNER)
(define-data-var next-loan-id uint u1)

;; Data maps
;; Map to store user credit scores
(define-map user-scores
  { user: principal }
  { 
    score: uint,
    total-loans: uint,
    active-loans: uint,
    completed-loans: uint,
    defaulted-loans: uint,
    last-updated: uint
  }
)

;; Map to store loan details
(define-map loans
  { loan-id: uint }
  {
    borrower: principal,
    amount: uint,
    duration-days: uint,
    interest-rate: uint,
    start-time: uint,
    due-time: uint,
    status: (string-ascii 20),  ;; "active", "completed", "defaulted"
    amount-repaid: uint,
    last-repayment: uint
  }
)

;; Map to store user loan history
(define-map user-loans
  { user: principal }
  { loan-ids: (list 50 uint) }
)

;; Public functions

;; Initialize a user's credit score
(define-public (initialize-user)
  (let ((user-exists (is-some (map-get? user-scores { user: tx-sender }))))
    (if user-exists
        (ok true)  ;; User already exists
        (begin
          (map-set user-scores
            { user: tx-sender }
            {
              score: INITIAL-SCORE,
              total-loans: u0,
              active-loans: u0,
              completed-loans: u0,
              defaulted-loans: u0,
              last-updated: stacks-block-height
            }
          )
          (map-set user-loans
            { user: tx-sender }
            { loan-ids: (list) }
          )
          (ok true)
        )
    )
  )
)

;; Create a new loan
(define-public (create-loan (amount uint) (duration-days uint) (interest-rate uint))
  (let (
        (loan-id (var-get next-loan-id))
        (user-data (default-to 
                    {
                      score: INITIAL-SCORE,
                      total-loans: u0,
                      active-loans: u0,
                      completed-loans: u0,
                      defaulted-loans: u0,
                      last-updated: stacks-block-height
                    } 
                    (map-get? user-scores { user: tx-sender })))
        (user-loan-data (default-to { loan-ids: (list) } (map-get? user-loans { user: tx-sender })))
      )
    
    ;; Validate inputs
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> duration-days u0) ERR-INVALID-DURATION)
    
    ;; Create the loan
    (map-set loans
      { loan-id: loan-id }
      {
        borrower: tx-sender,
        amount: amount,
        duration-days: duration-days,
        interest-rate: interest-rate,
        start-time: stacks-block-height,
        due-time: (+ stacks-block-height (* duration-days u144)), ;; Assuming ~144 blocks per day
        status: "active",
        amount-repaid: u0,
        last-repayment: u0
      }
    )
    
    ;; Update user data
    (map-set user-scores
      { user: tx-sender }
      {
        score: (get score user-data),
        total-loans: (+ (get total-loans user-data) u1),
        active-loans: (+ (get active-loans user-data) u1),
        completed-loans: (get completed-loans user-data),
        defaulted-loans: (get defaulted-loans user-data),
        last-updated: stacks-block-height
      }
    )
    
    ;; Update user loan history
    (map-set user-loans
      { user: tx-sender }
      { loan-ids: (unwrap! (as-max-len? (append (get loan-ids user-loan-data) loan-id) u50) ERR-NOT-AUTHORIZED) }
    )
    
    ;; Increment loan ID
    (var-set next-loan-id (+ loan-id u1))
    
    (ok loan-id)
  )
)

;; Record a loan repayment
(define-public (repay-loan (loan-id uint) (amount uint))
  (let (
        (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
        (user-data (unwrap! (map-get? user-scores { user: tx-sender }) ERR-NOT-AUTHORIZED))
      )
    
    ;; Validate the loan belongs to the sender
    (asserts! (is-eq (get borrower loan) tx-sender) ERR-NOT-AUTHORIZED)
    ;; Validate the loan is active
    (asserts! (is-eq (get status loan) "active") ERR-LOAN-NOT-ACTIVE)
    ;; Validate the amount
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (<= (+ (get amount-repaid loan) amount) (get amount loan)) ERR-REPAYMENT-EXCEEDS-DEBT)
    
    (let (
          (new-amount-repaid (+ (get amount-repaid loan) amount))
          (loan-completed (is-eq new-amount-repaid (get amount loan)))
          (new-status (if loan-completed "completed" "active"))
        )
      
      ;; Update loan data
      (map-set loans
        { loan-id: loan-id }
        {
          borrower: (get borrower loan),
          amount: (get amount loan),
          duration-days: (get duration-days loan),
          interest-rate: (get interest-rate loan),
          start-time: (get start-time loan),
          due-time: (get due-time loan),
          status: new-status,
          amount-repaid: new-amount-repaid,
          last-repayment: stacks-block-height
        }
      )
      
      ;; If loan is completed, update user score
      (if loan-completed
        (let (
              (on-time-completion (< stacks-block-height (get due-time loan)))
              (score-boost (if on-time-completion u30 u15))
              (new-score (min-value (+ (get score user-data) score-boost) MAX-SCORE))
              (new-active-loans (- (get active-loans user-data) u1))
              (new-completed-loans (+ (get completed-loans user-data) u1))
            )
          
          (map-set user-scores
            { user: tx-sender }
            {
              score: new-score,
              total-loans: (get total-loans user-data),
              active-loans: new-active-loans,
              completed-loans: new-completed-loans,
              defaulted-loans: (get defaulted-loans user-data),
              last-updated: stacks-block-height
            }
          )
        )
        true
      )
      
      (ok loan-completed)
    )
  )
)

;; Mark a loan as defaulted (admin only)
(define-public (mark-loan-defaulted (loan-id uint))
  (let (
        (admin-principal (var-get admin))
        (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      )
    
    ;; Only admin can mark loans as defaulted
    (asserts! (is-eq tx-sender admin-principal) ERR-NOT-AUTHORIZED)
    ;; Validate the loan is active
    (asserts! (is-eq (get status loan) "active") ERR-LOAN-NOT-ACTIVE)
    
    (let (
          (borrower (get borrower loan))
          (user-data (unwrap! (map-get? user-scores { user: borrower }) ERR-NOT-AUTHORIZED))
          (penalty (if (< (get score user-data) u600) u100 u50))
          (new-score (max-value u0 (- (get score user-data) penalty)))
        )
      
      ;; Update loan status
      (map-set loans
        { loan-id: loan-id }
        {
          borrower: (get borrower loan),
          amount: (get amount loan),
          duration-days: (get duration-days loan),
          interest-rate: (get interest-rate loan),
          start-time: (get start-time loan),
          due-time: (get due-time loan),
          status: "defaulted",
          amount-repaid: (get amount-repaid loan),
          last-repayment: (get last-repayment loan)
        }
      )
      
      ;; Update user score
      (map-set user-scores
        { user: borrower }
        {
          score: new-score,
          total-loans: (get total-loans user-data),
          active-loans: (- (get active-loans user-data) u1),
          completed-loans: (get completed-loans user-data),
          defaulted-loans: (+ (get defaulted-loans user-data) u1),
          last-updated: stacks-block-height
        }
      )
      
      (ok true)
    )
  )
)

;; Change admin (only current admin can do this)
(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (var-set admin new-admin)
    (ok true)
  )
)

;; Helper functions
(define-private (min-value (a uint) (b uint))
  (if (<= a b)
      a
      b))

(define-private (max-value (a uint) (b uint))
  (if (>= a b)
      a
      b))

;; Read only functions

;; Get user credit score
(define-read-only (get-credit-score (user principal))
  (let ((user-data (map-get? user-scores { user: user })))
    (if (is-some user-data)
        (ok (get score (unwrap-panic user-data)))
        (err ERR-NOT-AUTHORIZED)
    )
  )
)

;; Get user credit profile
(define-read-only (get-credit-profile (user principal))
  (let ((user-data (map-get? user-scores { user: user })))
    (if (is-some user-data)
        (ok (unwrap-panic user-data))
        (err ERR-NOT-AUTHORIZED)
    )
  )
)

;; Get loan details
(define-read-only (get-loan-details (loan-id uint))
  (let ((loan (map-get? loans { loan-id: loan-id })))
    (if (is-some loan)
        (ok (unwrap-panic loan))
        (err ERR-LOAN-NOT-FOUND)
    )
  )
)

;; Get user's loan history
(define-read-only (get-user-loans (user principal))
  (let ((user-loan-data (map-get? user-loans { user: user })))
    (if (is-some user-loan-data)
        (ok (get loan-ids (unwrap-panic user-loan-data)))
        (err ERR-NOT-AUTHORIZED)
    )
  )
)

;; Get recommended interest rate based on credit score
(define-read-only (get-recommended-interest-rate (user principal))
  (let ((user-data (map-get? user-scores { user: user })))
    (if (is-some user-data)
        (let ((score (get score (unwrap-panic user-data))))
          (ok (if (>= score SCORE-EXCELLENT) 
                u500  ;; 5.00%
                (if (>= score SCORE-GOOD)
                    u750  ;; 7.50%
                    (if (>= score SCORE-FAIR)
                        u1000  ;; 10.00%
                        (if (>= score SCORE-POOR)
                            u1500  ;; 15.00%
                            u2000  ;; 20.00%
                        )
                    )
                )
              ))
        )
        (err ERR-NOT-AUTHORIZED)
    )
  )
)

;; Get recommended borrowing limit based on credit score
(define-read-only (get-recommended-borrowing-limit (user principal))
  (let ((user-data (map-get? user-scores { user: user })))
    (if (is-some user-data)
        (let ((score (get score (unwrap-panic user-data))))
          (ok (if (>= score SCORE-EXCELLENT)
                u10000000  ;; 10,000,000 microSTX
                (if (>= score SCORE-GOOD)
                    u5000000  ;; 5,000,000 microSTX
                    (if (>= score SCORE-FAIR)
                        u2000000  ;; 2,000,000 microSTX
                        (if (>= score SCORE-POOR)
                            u500000  ;; 500,000 microSTX
                            u100000  ;; 100,000 microSTX
                        )
                    )
                )))
        )
        (err ERR-NOT-AUTHORIZED)
    )
  )
)

;; Check if a loan is overdue
(define-read-only (is-loan-overdue (loan-id uint))
  (let ((loan (map-get? loans { loan-id: loan-id })))
    (if (is-some loan)
        (let ((loan-data (unwrap-panic loan)))
          (ok (and 
                (is-eq (get status loan-data) "active")
                (> stacks-block-height (get due-time loan-data))
              ))
        )
        (err ERR-LOAN-NOT-FOUND)
    )
  )
)
