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
    last-repayment: uint,
    collateral-amount: uint,
    collateral-claimed: bool,
    extensions-used: uint      ;; New field

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
        last-repayment: u0,
        collateral-amount: u0,
        collateral-claimed: false,
        extensions-used: u0

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
          last-repayment: stacks-block-height,
          collateral-amount: (get collateral-amount loan),
          collateral-claimed: (get collateral-claimed loan),
          extensions-used: u0

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
          (loan (unwrap! (map-get? loanss { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
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
          last-repayment: (get last-repayment loan),
          collateral-amount: (get collateral-amount loan),
          collateral-claimed: (get collateral-claimed loan),
          extensions-used: u0

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


;; Add to constants section
(define-constant ERR-INSUFFICIENT-COLLATERAL (err u108))
(define-constant ERR-COLLATERAL-ALREADY-CLAIMED (err u109))

;; Update loans map to include collateral
(define-map loanss
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
    last-repayment: uint,
    collateral-amount: uint,    ;; New field
    collateral-claimed: bool    ;; New field
  }
)

;; Create a loan with collateral
(define-public (create-collateralized-loan (amount uint) (duration-days uint) (interest-rate uint) (collateral-amount uint))
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
    (asserts! (>= collateral-amount (/ amount u4)) ERR-INSUFFICIENT-COLLATERAL) ;; Collateral must be at least 25% of loan
    
    ;; Transfer collateral to contract (assuming STX)
    (try! (stx-transfer? collateral-amount tx-sender (as-contract tx-sender)))
    
    ;; Create the loan
    (map-set loans
      { loan-id: loan-id }
      {
        borrower: tx-sender,
        amount: amount,
        duration-days: duration-days,
        interest-rate: interest-rate,
        start-time: stacks-block-height,
        due-time: (+ stacks-block-height (* duration-days u144)),
        status: "active",
        amount-repaid: u0,
        last-repayment: u0,
        collateral-amount: collateral-amount,
        collateral-claimed: false,
        extensions-used: u0

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

;; Return collateral when loan is repaid
(define-public (return-collateral (loan-id uint))
  (let (
        (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      )
    
    ;; Validate the loan belongs to the sender
    (asserts! (is-eq (get borrower loan) tx-sender) ERR-NOT-AUTHORIZED)
    ;; Validate the loan is completed
    (asserts! (is-eq (get status loan) "completed") ERR-LOAN-NOT-ACTIVE)
    ;; Validate collateral hasn't been claimed
    ;; (asserts! (not (get collateral-claimed loan)) ERR-COLLATERAL-ALREADY-CLAIMED)
    
    ;; Return collateral to borrower
    ;; (try! (as-contract (stx-transfer? (get collateral loan) tx-sender (get borrower loan))))
    
    ;; Update loan to mark collateral as claimed
    ;; (map-set loans
    ;;   { loan-id: loan-id }
    ;;   (merge loan { collateral-claimed: true })
    ;; )
    
    (ok true)
  )
)

;; Claim collateral when loan is defaulted (admin only)
(define-public (claim-defaulted-collateral (loan-id uint))
  (let (
        (admin-principal (var-get admin))
        (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
      )
    
    ;; Only admin can claim collateral
    (asserts! (is-eq tx-sender admin-principal) ERR-NOT-AUTHORIZED)
    ;; Validate the loan is defaulted
    (asserts! (is-eq (get status loan) "defaulted") ERR-LOAN-NOT-ACTIVE)
    ;; Validate collateral hasn't been claimed
    (asserts! (not (get collateral-claimed loan)) ERR-COLLATERAL-ALREADY-CLAIMED)
    
    ;; Update loan to mark collateral as claimed
    (map-set loans
      { loan-id: loan-id }
      (merge loan { collateral-claimed: true })
    )
    
    (ok true)
  )
)



;; Add to constants section
(define-constant ERR-EXTENSION-NOT-ALLOWED (err u110))
(define-constant MAX-EXTENSIONS u3)


;; Request a loan extension
(define-public (extend-loan (loan-id uint) (additional-days uint))
  (let (
        (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
        (user-data (unwrap! (map-get? user-scores { user: (get borrower loan) }) ERR-NOT-AUTHORIZED))
      )
    
    ;; Validate the loan belongs to the sender
    (asserts! (is-eq (get borrower loan) tx-sender) ERR-NOT-AUTHORIZED)
    ;; Validate the loan is active
    (asserts! (is-eq (get status loan) "active") ERR-LOAN-NOT-ACTIVE)
    ;; Validate extension days
    (asserts! (> additional-days u0) ERR-INVALID-DURATION)
    ;; Validate extensions limit
    (asserts! (< (get extensions-used loan) MAX-EXTENSIONS) ERR-EXTENSION-NOT-ALLOWED)
    
    (let (
          (new-due-time (+ (get due-time loan) (* additional-days u144)))
          (new-extensions-used (+ (get extensions-used loan) u1))
          (score-penalty u10)
          (new-score (max-value u0 (- (get score user-data) score-penalty)))
        )
      
      ;; Update loan data
      (map-set loans
        { loan-id: loan-id }
        (merge loan {
          due-time: new-due-time,
          extensions-used: new-extensions-used
        })
      )
      
      ;; Update user score with penalty
      (map-set user-scores
        { user: tx-sender }
        (merge user-data {
          score: new-score,
          last-updated: stacks-block-height
        })
      )
      
      (ok new-due-time)
    )
  )
)



;; Add to constants section
(define-constant ERR-DISPUTE-ALREADY-EXISTS (err u111))
(define-constant ERR-DISPUTE-NOT-FOUND (err u112))
(define-constant ERR-INVALID-RESOLUTION (err u113))

;; Define dispute status types
(define-constant DISPUTE-STATUS-PENDING "pending")
(define-constant DISPUTE-STATUS-APPROVED "approved")
(define-constant DISPUTE-STATUS-REJECTED "rejected")

;; Map to store credit score disputes
(define-map credit-disputes
  { user: principal, dispute-id: uint }
  {
    reason: (string-ascii 100),
    requested-score: uint,
    current-score: uint,
    status: (string-ascii 20),
    created-at: uint,
    resolved-at: uint
  }
)

;; Map to track user's dispute count
(define-map user-disputes
  { user: principal }
  { 
    count: uint,
    active-dispute: bool
  }
)

;; File a credit score dispute
(define-public (file-dispute (reason (string-ascii 100)) (requested-score uint))
  (let (
        (user-data (unwrap! (map-get? user-scores { user: tx-sender }) ERR-NOT-AUTHORIZED))
        (dispute-data (default-to { count: u0, active-dispute: false } (map-get? user-disputes { user: tx-sender })))
      )
    
    ;; Validate no active dispute
    (asserts! (not (get active-dispute dispute-data)) ERR-DISPUTE-ALREADY-EXISTS)
    ;; Validate requested score
    (asserts! (<= requested-score MAX-SCORE) ERR-INVALID-SCORE-PARAMS)
    
    (let (
          (dispute-id (+ (get count dispute-data) u1))
        )
      
      ;; Create dispute
      (map-set credit-disputes
        { user: tx-sender, dispute-id: dispute-id }
        {
          reason: reason,
          requested-score: requested-score,
          current-score: (get score user-data),
          status: DISPUTE-STATUS-PENDING,
          created-at: stacks-block-height,
          resolved-at: u0
        }
      )
      
      ;; Update user dispute data
      (map-set user-disputes
        { user: tx-sender }
        {
          count: dispute-id,
          active-dispute: true
        }
      )
      
      (ok dispute-id)
    )
  )
)

;; Resolve a credit score dispute (admin only)
(define-public (resolve-dispute (user principal) (dispute-id uint) (approved bool) (new-score uint))
  (let (
        (admin-principal (var-get admin))
        (dispute (unwrap! (map-get? credit-disputes { user: user, dispute-id: dispute-id }) ERR-DISPUTE-NOT-FOUND))
        (user-data (unwrap! (map-get? user-scores { user: user }) ERR-NOT-AUTHORIZED))
        (dispute-data (unwrap! (map-get? user-disputes { user: user }) ERR-DISPUTE-NOT-FOUND))
      )
    
    ;; Only admin can resolve disputes
    (asserts! (is-eq tx-sender admin-principal) ERR-NOT-AUTHORIZED)
    ;; Validate dispute is pending
    (asserts! (is-eq (get status dispute) DISPUTE-STATUS-PENDING) ERR-INVALID-RESOLUTION)
    ;; Validate new score if approved
    (asserts! (or (not approved) (<= new-score MAX-SCORE)) ERR-INVALID-SCORE-PARAMS)
    
    (let (
          (resolution-status (if approved DISPUTE-STATUS-APPROVED DISPUTE-STATUS-REJECTED))
          (final-score (if approved new-score (get score user-data)))
        )
      
      ;; Update dispute
      (map-set credit-disputes
        { user: user, dispute-id: dispute-id }
        (merge dispute {
          status: resolution-status,
          resolved-at: stacks-block-height
        })
      )
      
      ;; Update user dispute data
      (map-set user-disputes
        { user: user }
        (merge dispute-data {
          active-dispute: false
        })
      )
      
      ;; Update user score if approved
      (if approved
          (map-set user-scores
            { user: user }
            (merge user-data {
              score: final-score,
              last-updated: stacks-block-height
            })
          )
          true
      )
      
      (ok final-score)
    )
  )
)

;; Get dispute details
(define-read-only (get-dispute-details (user principal) (dispute-id uint))
  (let ((dispute (map-get? credit-disputes { user: user, dispute-id: dispute-id })))
    (if (is-some dispute)
        (ok (unwrap-panic dispute))
        (err ERR-DISPUTE-NOT-FOUND)
    )
  )
)


(define-constant RECOVERY-MIN-STAKE u1000000)
(define-constant RECOVERY-MIN-BLOCKS u14400)
(define-constant RECOVERY-SCORE-BOOST u5)
(define-constant ERR-INSUFFICIENT-STAKE (err u120))
(define-constant ERR-ALREADY-IN-RECOVERY (err u121))

(define-map recovery-programs
  { user: principal }
  {
    stake-amount: uint,
    start-block: uint,
    last-boost: uint
  }
)

(define-public (start-recovery-program (stake-amount uint))
  (let (
    (user-data (unwrap! (map-get? user-scores { user: tx-sender }) ERR-NOT-AUTHORIZED))
    (existing-program (map-get? recovery-programs { user: tx-sender }))
    )
    
    (asserts! (is-none existing-program) ERR-ALREADY-IN-RECOVERY)
    (asserts! (>= stake-amount RECOVERY-MIN-STAKE) ERR-INSUFFICIENT-STAKE)
    
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    
    (map-set recovery-programs
      { user: tx-sender }
      {
        stake-amount: stake-amount,
        start-block: stacks-block-height,
        last-boost: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (claim-recovery-boost)
  (let (
    (program (unwrap! (map-get? recovery-programs { user: tx-sender }) ERR-NOT-AUTHORIZED))
    (user-data (unwrap! (map-get? user-scores { user: tx-sender }) ERR-NOT-AUTHORIZED))
    (blocks-staked (- stacks-block-height (get last-boost program)))
    )
    
    (asserts! (>= blocks-staked RECOVERY-MIN-BLOCKS) ERR-NOT-AUTHORIZED)
    
    (map-set user-scores
      { user: tx-sender }
      (merge user-data {
        score: (min-value (+ (get score user-data) RECOVERY-SCORE-BOOST) MAX-SCORE),
        last-updated: stacks-block-height
      })
    )
    
    (map-set recovery-programs
      { user: tx-sender }
      (merge program { last-boost: stacks-block-height })
    )
    (ok true)
  )
)


(define-constant INSURANCE-COST u100000) 
(define-constant INSURANCE-DURATION u14400)
(define-constant INSURANCE-THRESHOLD u50)
(define-constant ERR-INSURANCE-EXISTS (err u130))

(define-map credit-insurance
  { user: principal }
  {
    start-block: uint,
    end-block: uint,
    base-score: uint
  }
)

(define-public (purchase-insurance)
  (let (
    (user-data (unwrap! (map-get? user-scores { user: tx-sender }) ERR-NOT-AUTHORIZED))
    (existing-insurance (map-get? credit-insurance { user: tx-sender }))
    )
    
    (asserts! (is-none existing-insurance) ERR-INSURANCE-EXISTS)
    (try! (stx-transfer? INSURANCE-COST tx-sender (as-contract tx-sender)))
    
    (map-set credit-insurance
      { user: tx-sender }
      {
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height INSURANCE-DURATION),
        base-score: (get score user-data)
      }
    )
    (ok true)
  )
)

(define-public (claim-insurance)
  (let (
    (insurance (unwrap! (map-get? credit-insurance { user: tx-sender }) ERR-NOT-AUTHORIZED))
    (user-data (unwrap! (map-get? user-scores { user: tx-sender }) ERR-NOT-AUTHORIZED))
    )
    
    (asserts! (<= stacks-block-height (get end-block insurance)) ERR-NOT-AUTHORIZED)
    (asserts! (>= (- (get base-score insurance) (get score user-data)) INSURANCE-THRESHOLD) ERR-NOT-AUTHORIZED)
    
    (map-set user-scores
      { user: tx-sender } 
      (merge user-data {
        score: (get base-score insurance),
        last-updated: stacks-block-height
      })
    )
    
    (map-delete credit-insurance { user: tx-sender })
    (ok true)
  )
)

(define-constant ERR-VERIFIER-NOT-AUTHORIZED (err u140))
(define-constant ERR-ATTESTATION-NOT-FOUND (err u141))
(define-constant ERR-ATTESTATION-EXPIRED (err u142))
(define-constant ERR-INVALID-ATTESTATION (err u143))

(define-constant ATTESTATION-DURATION u43200)
(define-constant MAX-ATTESTATION-IMPACT u100)
(define-constant MIN-ATTESTATION-IMPACT u10)

(define-map authorized-verifiers
  { verifier: principal }
  {
    name: (string-ascii 50),
    reputation-score: uint,
    total-attestations: uint,
    active: bool,
    authorized-at: uint
  }
)

(define-map credit-attestations
  { user: principal, verifier: principal, attestation-id: uint }
  {
    score-impact: int,
    confidence-level: uint,
    attestation-type: (string-ascii 30),
    created-at: uint,
    expires-at: uint,
    verified: bool,
    metadata: (string-ascii 100)
  }
)

(define-map user-attestation-summary
  { user: principal }
  {
    total-attestations: uint,
    positive-attestations: uint,
    negative-attestations: uint,
    last-attestation: uint,
    attestation-score-boost: int
  }
)

(define-data-var next-attestation-id uint u1)

(define-public (authorize-verifier (verifier principal) (name (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (map-set authorized-verifiers
      { verifier: verifier }
      {
        name: name,
        reputation-score: u100,
        total-attestations: u0,
        active: true,
        authorized-at: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (revoke-verifier (verifier principal))
  (let (
    (verifier-data (unwrap! (map-get? authorized-verifiers { verifier: verifier }) ERR-VERIFIER-NOT-AUTHORIZED))
    )
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (map-set authorized-verifiers
      { verifier: verifier }
      (merge verifier-data { active: false })
    )
    (ok true)
  )
)

(define-public (create-attestation (user principal) (score-impact int) (confidence-level uint) (attestation-type (string-ascii 30)) (metadata (string-ascii 100)))
  (let (
    (verifier-data (unwrap! (map-get? authorized-verifiers { verifier: tx-sender }) ERR-VERIFIER-NOT-AUTHORIZED))
    (attestation-id (var-get next-attestation-id))
    (user-summary (default-to 
      {
        total-attestations: u0,
        positive-attestations: u0,
        negative-attestations: u0,
        last-attestation: u0,
        attestation-score-boost: 0
      }
      (map-get? user-attestation-summary { user: user })
    ))
    )
    
    (asserts! (get active verifier-data) ERR-VERIFIER-NOT-AUTHORIZED)
    ;; (asserts! (and (>= score-impact (- MAX-ATTESTATION-IMPACT)) (<= score-impact (to-int MAX-ATTESTATION-IMPACT))) ERR-INVALID-ATTESTATION)
    (asserts! (and (>= confidence-level u1) (<= confidence-level u100)) ERR-INVALID-ATTESTATION)
    
    (map-set credit-attestations
      { user: user, verifier: tx-sender, attestation-id: attestation-id }
      {
        score-impact: score-impact,
        confidence-level: confidence-level,
        attestation-type: attestation-type,
        created-at: stacks-block-height,
        expires-at: (+ stacks-block-height ATTESTATION-DURATION),
        verified: true,
        metadata: metadata
      }
    )
    
    (map-set authorized-verifiers
      { verifier: tx-sender }
      (merge verifier-data { total-attestations: (+ (get total-attestations verifier-data) u1) })
    )
    
    (let (
      (is-positive (> score-impact 0))
      (new-positive (if is-positive (+ (get positive-attestations user-summary) u1) (get positive-attestations user-summary)))
      (new-negative (if is-positive (get negative-attestations user-summary) (+ (get negative-attestations user-summary) u1)))
      )
      
      (map-set user-attestation-summary
        { user: user }
        {
          total-attestations: (+ (get total-attestations user-summary) u1),
          positive-attestations: new-positive,
          negative-attestations: new-negative,
          last-attestation: stacks-block-height,
          attestation-score-boost: (+ (get attestation-score-boost user-summary) score-impact)
        }
      )
    )
    
    (var-set next-attestation-id (+ attestation-id u1))
    (ok attestation-id)
  )
)

(define-public (apply-attestation-boost (user principal))
  (let (
    (user-data (unwrap! (map-get? user-scores { user: user }) ERR-NOT-AUTHORIZED))
    (attestation-summary (unwrap! (map-get? user-attestation-summary { user: user }) ERR-ATTESTATION-NOT-FOUND))
    (current-boost (get attestation-score-boost attestation-summary))
    (current-score (get score user-data))
    )
    
    (asserts! (is-eq tx-sender user) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-eq current-boost 0)) ERR-INVALID-ATTESTATION)
    
    (let (
      (new-score (if (> current-boost 0)
        (min-value (+ current-score (to-uint current-boost)) MAX-SCORE)
        (max-value (if (>= current-score (to-uint (- current-boost))) (- current-score (to-uint (- current-boost))) u0) u0)
      ))
      )
      
      (map-set user-scores
        { user: user }
        (merge user-data {
          score: new-score,
          last-updated: stacks-block-height
        })
      )
      
      (map-set user-attestation-summary
        { user: user }
        (merge attestation-summary { attestation-score-boost: 0 })
      )
      
      (ok new-score)
    )
  )
)

(define-read-only (get-verifier-info (verifier principal))
  (let ((verifier-data (map-get? authorized-verifiers { verifier: verifier })))
    (if (is-some verifier-data)
      (ok (unwrap-panic verifier-data))
      (err ERR-VERIFIER-NOT-AUTHORIZED)
    )
  )
)

(define-read-only (get-attestation-details (user principal) (verifier principal) (attestation-id uint))
  (let ((attestation (map-get? credit-attestations { user: user, verifier: verifier, attestation-id: attestation-id })))
    (if (is-some attestation)
      (ok (unwrap-panic attestation))
      (err ERR-ATTESTATION-NOT-FOUND)
    )
  )
)

(define-read-only (get-user-attestation-summary (user principal))
  (let ((summary (map-get? user-attestation-summary { user: user })))
    (if (is-some summary)
      (ok (unwrap-panic summary))
      (err ERR-ATTESTATION-NOT-FOUND)
    )
  )
)

(define-read-only (calculate-attestation-weighted-score (user principal))
  (let (
    (user-data (map-get? user-scores { user: user }))
    (attestation-summary (map-get? user-attestation-summary { user: user }))
    )
    
    (if (and (is-some user-data) (is-some attestation-summary))
      (let (
        (base-score (get score (unwrap-panic user-data)))
        (attestation-boost (get attestation-score-boost (unwrap-panic attestation-summary)))
        (total-attestations (get total-attestations (unwrap-panic attestation-summary)))
        )
        
        (if (> total-attestations u0)
          (let (
            (weighted-boost (/ attestation-boost (to-int total-attestations)))
            (final-score (if (> weighted-boost 0)
              (min-value (+ base-score (to-uint weighted-boost)) MAX-SCORE)
              (max-value (if (>= base-score (to-uint (- weighted-boost))) (- base-score (to-uint (- weighted-boost))) u0) u0)
            ))
            )
            (ok final-score)
          )
          (ok base-score)
        )
      )
      (err ERR-NOT-AUTHORIZED)
    )
  )
)

(define-read-only (is-attestation-valid (user principal) (verifier principal) (attestation-id uint))
  (let ((attestation (map-get? credit-attestations { user: user, verifier: verifier, attestation-id: attestation-id })))
    (if (is-some attestation)
      (let ((attestation-data (unwrap-panic attestation)))
        (ok (and 
          (get verified attestation-data)
          (< stacks-block-height (get expires-at attestation-data))
        ))
      )
      (err ERR-ATTESTATION-NOT-FOUND)
    )
  )
)

;; === PREDICTIVE RISK ASSESSMENT ENGINE ===

;; Risk prediction constants
(define-constant PREDICTION-WINDOW u30240) ;; ~6 months in blocks
(define-constant MIN-DATA-POINTS u3)
(define-constant RISK-EXCELLENT u10) ;; 1-10% risk
(define-constant RISK-LOW u25) ;; 11-25% risk  
(define-constant RISK-MODERATE u50) ;; 26-50% risk
(define-constant RISK-HIGH u75) ;; 51-75% risk
(define-constant RISK-EXTREME u100) ;; 76-100% risk

;; Error constants for risk engine
(define-constant ERR-INSUFFICIENT-HISTORY (err u300))
(define-constant ERR-PREDICTION-EXISTS (err u301))
(define-constant ERR-PREDICTION-NOT-FOUND (err u302))

;; User behavior tracking for predictions
(define-map user-behavior-patterns
  { user: principal }
  {
    avg-payment-delay: uint, ;; Average blocks late
    payment-consistency: uint, ;; 0-100 consistency score
    loan-frequency: uint, ;; Loans per time period
    repayment-velocity: uint, ;; Speed of repayments
    score-trajectory: int, ;; Score change trend
    risk-factors: uint, ;; Accumulated risk indicators
    last-analysis: uint
  }
)

;; Credit predictions with confidence levels
(define-map credit-predictions
  { user: principal, prediction-id: uint }
  {
    predicted-score: uint,
    confidence-level: uint, ;; 0-100 confidence
    risk-category: (string-ascii 20),
    risk-percentage: uint,
    prediction-horizon: uint, ;; Blocks into future
    key-factors: (string-ascii 100),
    created-at: uint,
    expires-at: uint,
    accuracy-tracked: bool
  }
)

;; Risk assessment profiles
(define-map risk-profiles
  { user: principal }
  {
    overall-risk-score: uint,
    default-probability: uint,
    recommended-max-loan: uint,
    risk-adjusted-rate: uint,
    stability-index: uint,
    last-updated: uint
  }
)

;; Prediction accuracy tracking
(define-map prediction-accuracy
  { prediction-id: uint }
  {
    actual-score: uint,
    predicted-score: uint,
    accuracy-percentage: uint,
    prediction-error: uint
  }
)

(define-data-var next-prediction-id uint u1)

;; Analyze user behavior patterns for risk assessment
(define-public (analyze-behavior-pattern (user principal))
  (let (
    (user-data (unwrap! (map-get? user-scores { user: user }) ERR-NOT-AUTHORIZED))
    (user-loan-data (default-to { loan-ids: (list) } (map-get? user-loans { user: user })))
    )
    
    ;; Need minimum history to analyze
    (asserts! (>= (get total-loans user-data) MIN-DATA-POINTS) ERR-INSUFFICIENT-HISTORY)
    
    (let (
      ;; Calculate behavior metrics
      (avg-delay (calculate-avg-payment-delay user))
      (consistency (calculate-payment-consistency user-data))
      (frequency (calculate-loan-frequency user-data))
      (velocity (calculate-repayment-velocity user))
      (trajectory (calculate-score-trajectory user-data))
      (risk-factors (calculate-risk-factors user-data avg-delay))
      )
      
      ;; Store behavior pattern
      (map-set user-behavior-patterns
        { user: user }
        {
          avg-payment-delay: avg-delay,
          payment-consistency: consistency,
          loan-frequency: frequency,
          repayment-velocity: velocity,
          score-trajectory: trajectory,
          risk-factors: risk-factors,
          last-analysis: stacks-block-height
        }
      )
      
      (ok true)
    )
  )
)

;; Generate credit score prediction
(define-public (generate-prediction (user principal) (horizon-blocks uint))
  (let (
    (pattern (unwrap! (map-get? user-behavior-patterns { user: user }) ERR-INSUFFICIENT-HISTORY))
    (user-data (unwrap! (map-get? user-scores { user: user }) ERR-NOT-AUTHORIZED))
    (prediction-id (var-get next-prediction-id))
    )
    
    (let (
      ;; Calculate prediction components
      (base-score (get score user-data))
      (trend-impact (calculate-trend-impact (get score-trajectory pattern) horizon-blocks))
      (risk-adjustment (calculate-risk-adjustment (get risk-factors pattern)))
      (predicted-score (calculate-predicted-score base-score trend-impact risk-adjustment))
      (confidence (calculate-confidence-level pattern))
      (risk-category (determine-risk-category (get risk-factors pattern)))
      (risk-percentage (calculate-risk-percentage pattern))
      )
      
      ;; Store prediction
      (map-set credit-predictions
        { user: user, prediction-id: prediction-id }
        {
          predicted-score: predicted-score,
          confidence-level: confidence,
          risk-category: risk-category,
          risk-percentage: risk-percentage,
          prediction-horizon: horizon-blocks,
          key-factors: (generate-key-factors pattern),
          created-at: stacks-block-height,
          expires-at: (+ stacks-block-height horizon-blocks),
          accuracy-tracked: false
        }
      )
      
      ;; Increment prediction ID
      (var-set next-prediction-id (+ prediction-id u1))
      
      (ok prediction-id)
    )
  )
)

;; Create comprehensive risk profile
(define-public (create-risk-profile (user principal))
  (let (
    (pattern (unwrap! (map-get? user-behavior-patterns { user: user }) ERR-INSUFFICIENT-HISTORY))
    (user-data (unwrap! (map-get? user-scores { user: user }) ERR-NOT-AUTHORIZED))
    )
    
    (let (
      (overall-risk (calculate-overall-risk pattern user-data))
      (default-prob (calculate-default-probability pattern))
      (max-loan (calculate-safe-loan-amount user-data overall-risk))
      (risk-rate (calculate-risk-adjusted-rate user-data overall-risk))
      (stability (calculate-stability-index pattern))
      )
      
      (map-set risk-profiles
        { user: user }
        {
          overall-risk-score: overall-risk,
          default-probability: default-prob,
          recommended-max-loan: max-loan,
          risk-adjusted-rate: risk-rate,
          stability-index: stability,
          last-updated: stacks-block-height
        }
      )
      
      (ok overall-risk)
    )
  )
)

;; Private helper functions for calculations

(define-private (calculate-avg-payment-delay (user principal))
  ;; Analyzes payment behavior patterns
  (let (
    (user-data (default-to 
      { score: u650, total-loans: u0, active-loans: u0, completed-loans: u0, defaulted-loans: u0, last-updated: u0 }
      (map-get? user-scores { user: user })
    ))
    )
    ;; Risk factor based on defaults and score
    (if (> (get defaulted-loans user-data) u0)
      (+ u100 (* (get defaulted-loans user-data) u50))
      (if (< (get score user-data) u600) u75 u25)
    )
  )
)

(define-private (calculate-payment-consistency (user-data { score: uint, total-loans: uint, active-loans: uint, completed-loans: uint, defaulted-loans: uint, last-updated: uint }))
  (let (
    (total-loans (get total-loans user-data))
    (completed-loans (get completed-loans user-data))
    (defaulted-loans (get defaulted-loans user-data))
    )
    (if (is-eq total-loans u0)
      u50 ;; Default consistency for new users
      (/ (* (- total-loans defaulted-loans) u100) total-loans)
    )
  )
)

(define-private (calculate-loan-frequency (user-data { score: uint, total-loans: uint, active-loans: uint, completed-loans: uint, defaulted-loans: uint, last-updated: uint }))
  ;; Frequency per time period
  (min-value (get total-loans user-data) u10)
)

(define-private (calculate-repayment-velocity (user principal))
  ;; Speed of repayments - baseline metric
  u50
)

(define-private (calculate-score-trajectory (user-data { score: uint, total-loans: uint, active-loans: uint, completed-loans: uint, defaulted-loans: uint, last-updated: uint }))
  ;; Score change trend analysis
  (let (
    (current-score (get score user-data))
    (defaults (get defaulted-loans user-data))
    )
    (if (> defaults u0)
      (- 0 (* (to-int defaults) 20)) ;; Negative trend
      (if (>= current-score u700) 10 5) ;; Positive trend
    )
  )
)

(define-private (calculate-risk-factors (user-data { score: uint, total-loans: uint, active-loans: uint, completed-loans: uint, defaulted-loans: uint, last-updated: uint }) (avg-delay uint))
  (let (
    (defaults (get defaulted-loans user-data))
    (score (get score user-data))
    (active-loans (get active-loans user-data))
    )
    (+ 
      (* defaults u30) ;; Each default adds 30 risk points
      (if (< score u600) u25 u0) ;; Low score adds risk
      (if (> active-loans u3) u15 u0) ;; Too many active loans
      (if (> avg-delay u50) u20 u0) ;; Payment delays
    )
  )
)

(define-private (calculate-trend-impact (trajectory int) (horizon uint))
  (* trajectory (to-int (/ horizon u5040))) ;; Impact over time periods
)

(define-private (calculate-risk-adjustment (risk-factors uint))
  (if (> risk-factors u75) (- 0 50) ;; High risk: negative adjustment
    (if (> risk-factors u50) (- 0 25) ;; Medium risk
      (if (> risk-factors u25) (- 0 10) ;; Low-medium risk
        0 ;; Low risk: no adjustment
      )
    )
  )
)

(define-private (calculate-predicted-score (base-score uint) (trend-impact int) (risk-adjustment int))
  (let (
    (raw-prediction (+ (to-int base-score) trend-impact risk-adjustment))
    )
    (to-uint (if (< raw-prediction 0) 0 (if (> raw-prediction 850) 850 raw-prediction)))
  )
)

(define-private (calculate-confidence-level (pattern { avg-payment-delay: uint, payment-consistency: uint, loan-frequency: uint, repayment-velocity: uint, score-trajectory: int, risk-factors: uint, last-analysis: uint }))
  (let (
    (consistency (get payment-consistency pattern))
    (frequency (get loan-frequency pattern))
    )
    ;; Higher consistency and more data = higher confidence
    (min-value u95 (+ consistency (min-value frequency u20)))
  )
)

(define-private (determine-risk-category (risk-factors uint))
  (if (<= risk-factors RISK-EXCELLENT) "Excellent"
    (if (<= risk-factors RISK-LOW) "Low"
      (if (<= risk-factors RISK-MODERATE) "Moderate"
        (if (<= risk-factors RISK-HIGH) "High"
          "Extreme"
        )
      )
    )
  )
)

(define-private (calculate-risk-percentage (pattern { avg-payment-delay: uint, payment-consistency: uint, loan-frequency: uint, repayment-velocity: uint, score-trajectory: int, risk-factors: uint, last-analysis: uint }))
  (min-value u100 (get risk-factors pattern))
)

(define-private (generate-key-factors (pattern { avg-payment-delay: uint, payment-consistency: uint, loan-frequency: uint, repayment-velocity: uint, score-trajectory: int, risk-factors: uint, last-analysis: uint }))
  (if (> (get risk-factors pattern) u50)
    "High risk factors detected"
    (if (< (get payment-consistency pattern) u70)
      "Inconsistent payment history"
      "Stable payment behavior"
    )
  )
)

(define-private (calculate-overall-risk (pattern { avg-payment-delay: uint, payment-consistency: uint, loan-frequency: uint, repayment-velocity: uint, score-trajectory: int, risk-factors: uint, last-analysis: uint }) (user-data { score: uint, total-loans: uint, active-loans: uint, completed-loans: uint, defaulted-loans: uint, last-updated: uint }))
  (let (
    (behavior-risk (get risk-factors pattern))
    (score-risk (if (< (get score user-data) u600) u30 u10))
    )
    (min-value u100 (+ behavior-risk score-risk))
  )
)

(define-private (calculate-default-probability (pattern { avg-payment-delay: uint, payment-consistency: uint, loan-frequency: uint, repayment-velocity: uint, score-trajectory: int, risk-factors: uint, last-analysis: uint }))
  (/ (get risk-factors pattern) u2) ;; Convert risk factors to probability
)

(define-private (calculate-safe-loan-amount (user-data { score: uint, total-loans: uint, active-loans: uint, completed-loans: uint, defaulted-loans: uint, last-updated: uint }) (overall-risk uint))
  (let (
    (base-limit (if (>= (get score user-data) u800) u10000000
                  (if (>= (get score user-data) u700) u5000000
                    (if (>= (get score user-data) u600) u2000000 u500000))))
    (risk-multiplier (if (> overall-risk u75) u50 ;; 50% of base for high risk
                      (if (> overall-risk u50) u75 ;; 75% for medium risk
                        u100))) ;; 100% for low risk
    )
    (/ (* base-limit risk-multiplier) u100)
  )
)

(define-private (calculate-risk-adjusted-rate (user-data { score: uint, total-loans: uint, active-loans: uint, completed-loans: uint, defaulted-loans: uint, last-updated: uint }) (overall-risk uint))
  (let (
    (base-rate (if (>= (get score user-data) u800) u500
                 (if (>= (get score user-data) u700) u750
                   (if (>= (get score user-data) u600) u1000 u1500))))
    (risk-premium (/ overall-risk u10)) ;; Add risk premium
    )
    (+ base-rate risk-premium)
  )
)

(define-private (calculate-stability-index (pattern { avg-payment-delay: uint, payment-consistency: uint, loan-frequency: uint, repayment-velocity: uint, score-trajectory: int, risk-factors: uint, last-analysis: uint }))
  (- u100 (min-value u100 (get risk-factors pattern)))
)

;; Read-only functions for accessing predictions and risk data

(define-read-only (get-behavior-pattern (user principal))
  (let (
    (pattern (map-get? user-behavior-patterns { user: user }))
    )
    (if (is-some pattern)
      (ok (unwrap-panic pattern))
      (err ERR-INSUFFICIENT-HISTORY)
    )
  )
)

(define-read-only (get-prediction (user principal) (prediction-id uint))
  (let (
    (prediction (map-get? credit-predictions { user: user, prediction-id: prediction-id }))
    )
    (if (is-some prediction)
      (ok (unwrap-panic prediction))
      (err ERR-PREDICTION-NOT-FOUND)
    )
  )
)

(define-read-only (get-risk-profile (user principal))
  (let (
    (profile (map-get? risk-profiles { user: user }))
    )
    (if (is-some profile)
      (ok (unwrap-panic profile))
      (err ERR-INSUFFICIENT-HISTORY)
    )
  )
)

(define-read-only (get-personalized-insights (user principal))
  (let (
    (pattern (map-get? user-behavior-patterns { user: user }))
    (profile (map-get? risk-profiles { user: user }))
    )
    (if (and (is-some pattern) (is-some profile))
      (let (
        (p (unwrap-panic pattern))
        (r (unwrap-panic profile))
        )
        (ok {
          risk-level: (determine-risk-category (get risk-factors p)),
          improvement-potential: (if (< (get overall-risk-score r) u25) "Low" 
                                  (if (< (get overall-risk-score r) u50) "Medium" "High")),
          recommended-action: (if (> (get risk-factors p) u50) "Focus on payment consistency" 
                               "Consider increasing credit utilization"),
          stability-score: (get stability-index r)
        })
      )
      (err ERR-INSUFFICIENT-HISTORY)
    )
  )
)






