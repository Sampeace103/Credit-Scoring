(define-constant ERR-BENCHMARKING-NOT-AUTHORIZED (err u200))
(define-constant ERR-INVALID-SCORE-RANGE (err u201))
(define-constant ERR-INSUFFICIENT-DATA (err u202))

(define-constant SCORE-RANGE-EXCELLENT u800)
(define-constant SCORE-RANGE-GOOD u700)
(define-constant SCORE-RANGE-FAIR u600)
(define-constant SCORE-RANGE-POOR u500)

(define-map score-range-stats
  { score-range: uint }
  {
    total-users: uint,
    avg-score: uint,
    avg-active-loans: uint,
    avg-completion-rate: uint,
    last-updated: uint
  }
)

(define-map user-benchmarks
  { user: principal }
  {
    score-percentile: uint,
    loans-percentile: uint,
    completion-percentile: uint,
    improvement-score: uint,
    benchmark-tier: (string-ascii 20),
    last-calculated: uint
  }
)

(define-data-var benchmarking-admin principal tx-sender)

(define-private (get-score-range (score uint))
  (if (>= score SCORE-RANGE-EXCELLENT)
    SCORE-RANGE-EXCELLENT
    (if (>= score SCORE-RANGE-GOOD)
      SCORE-RANGE-GOOD
      (if (>= score SCORE-RANGE-FAIR)
        SCORE-RANGE-FAIR
        (if (>= score SCORE-RANGE-POOR)
          SCORE-RANGE-POOR
          u0
        )
      )
    )
  )
)

(define-private (calculate-percentile (user-value uint) (avg-value uint) (range-factor uint))
  (let (
    (performance-ratio (if (is-eq avg-value u0) u50 (/ (* user-value u100) avg-value)))
    )
    (if (>= performance-ratio u150)
      u95
      (if (>= performance-ratio u125)
        u80
        (if (>= performance-ratio u110)
          u70
          (if (>= performance-ratio u100)
            u60
            (if (>= performance-ratio u90)
              u50
              (if (>= performance-ratio u75)
                u40
                (if (>= performance-ratio u60)
                  u30
                  (if (>= performance-ratio u40)
                    u20
                    u10
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

(define-private (get-tier-name (score uint))
  (if (>= score SCORE-RANGE-EXCELLENT)
    "Excellent"
    (if (>= score SCORE-RANGE-GOOD)
      "Good"
      (if (>= score SCORE-RANGE-FAIR)
        "Fair"
        (if (>= score SCORE-RANGE-POOR)
          "Poor"
          "Building"
        )
      )
    )
  )
)

(define-public (update-range-statistics (score-range uint) (user-count uint) (avg-score uint) (avg-active-loans uint) (avg-completion-rate uint))
  (begin
    (asserts! (is-eq tx-sender (var-get benchmarking-admin)) ERR-BENCHMARKING-NOT-AUTHORIZED)
    (asserts! (> user-count u0) ERR-INSUFFICIENT-DATA)
    
    (map-set score-range-stats
      { score-range: score-range }
      {
        total-users: user-count,
        avg-score: avg-score,
        avg-active-loans: avg-active-loans,
        avg-completion-rate: avg-completion-rate,
        last-updated: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (calculate-user-benchmark (user principal) (user-score uint) (user-active-loans uint) (user-completion-rate uint))
  (let (
    (score-range (get-score-range user-score))
    (range-stats (map-get? score-range-stats { score-range: score-range }))
    )
    
    (asserts! (is-some range-stats) ERR-INSUFFICIENT-DATA)
    
    (let (
      (stats (unwrap-panic range-stats))
      (score-percentile (calculate-percentile user-score (get avg-score stats) u1))
      (loans-percentile (calculate-percentile user-active-loans (get avg-active-loans stats) u1))
      (completion-percentile (calculate-percentile user-completion-rate (get avg-completion-rate stats) u1))
      (improvement-score (/ (+ score-percentile loans-percentile completion-percentile) u3))
      (benchmark-tier (get-tier-name user-score))
      )
      
      (map-set user-benchmarks
        { user: user }
        {
          score-percentile: score-percentile,
          loans-percentile: loans-percentile,
          completion-percentile: completion-percentile,
          improvement-score: improvement-score,
          benchmark-tier: benchmark-tier,
          last-calculated: stacks-block-height
        }
      )
      (ok improvement-score)
    )
  )
)

(define-read-only (get-user-benchmark (user principal))
  (let (
    (benchmark (map-get? user-benchmarks { user: user }))
    )
    (if (is-some benchmark)
      (ok (unwrap-panic benchmark))
      (err ERR-BENCHMARKING-NOT-AUTHORIZED)
    )
  )
)

(define-read-only (get-range-statistics (score-range uint))
  (let (
    (stats (map-get? score-range-stats { score-range: score-range }))
    )
    (if (is-some stats)
      (ok (unwrap-panic stats))
      (err ERR-INSUFFICIENT-DATA)
    )
  )
)

(define-read-only (get-user-ranking-summary (user principal))
  (let (
    (benchmark (unwrap! (map-get? user-benchmarks { user: user }) ERR-BENCHMARKING-NOT-AUTHORIZED))
    (score-percentile (get score-percentile benchmark))
    (improvement-score (get improvement-score benchmark))
    )
    (ok {
      overall-rank: (if (>= score-percentile u80) "Top Performer" 
                     (if (>= score-percentile u60) "Above Average"
                       (if (>= score-percentile u40) "Average"
                         "Below Average"
                       )
                     )
                   ),
      improvement-potential: (if (< improvement-score u40) "High" 
                              (if (< improvement-score u70) "Medium" 
                                "Low"
                              )
                            ),
      benchmark-tier: (get benchmark-tier benchmark),
      percentile-score: score-percentile
    })
  )
)

(define-read-only (compare-with-peers (user principal) (target-score uint))
  (let (
    (current-benchmark (unwrap! (map-get? user-benchmarks { user: user }) ERR-BENCHMARKING-NOT-AUTHORIZED))
    (target-range (get-score-range target-score))
    (target-stats (unwrap! (map-get? score-range-stats { score-range: target-range }) ERR-INSUFFICIENT-DATA))
    (current-score-percentile (get score-percentile current-benchmark))
    )
    (ok {
      target-tier: (get-tier-name target-score),
      target-avg-score: (get avg-score target-stats),
      target-user-count: (get total-users target-stats),
      score-gap: (if (> target-score current-score-percentile) (- target-score current-score-percentile) u0),
      achievable: (>= current-score-percentile u40)
    })
  )
)

(define-public (set-benchmarking-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get benchmarking-admin)) ERR-BENCHMARKING-NOT-AUTHORIZED)
    (var-set benchmarking-admin new-admin)
    (ok true)
  )
)
