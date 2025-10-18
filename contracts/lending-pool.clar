;; Errors
(define-constant ERR_INVALID_WITHDRAW_AMOUNT (err u100))
(define-constant ERR_EXCEEDED_MAX_BORROW (err u101))
(define-constant ERR_CANNOT_BE_LIQUIDATED (err u102))
(define-constant ERR_MUST_WITHDRAW_BEFORE_NEW_DEPOSIT (err u103))

;; Constants
(define-constant LTV_PERCENTAGE u70)
(define-constant INTEREST_RATE_PERCENTAGE u10)
(define-constant LIQUIDATION_THRESHOLD_PERCENTAGE u100)
(define-constant ONE_YEAR_IN_SECS u31556952)

;; Storage

;; Total amount of sBTC we have as collateral for loans
(define-data-var total-sbtc-collateral uint u0)

;; Total amount of STX that lenders have deposited
(define-data-var total-stx-deposits uint u1)

;; Total amount of STX that borrowers have borrowed
(define-data-var total-stx-borrows uint u0)

;; Last time interest was accrued
(define-data-var last-interest-accrual uint (get-latest-timestamp))

;; Cumulative interest earned (yield) in bips (interest * 10000)
(define-data-var cumulative-yield-bips uint u0)

;; Map of user principal to amount of sBTC they have as collateral
(define-map collateral
  { user: principal }
  { amount: uint }
)

;; Map of user principal to amount of STX they have deposited
(define-map deposits
  { user: principal }
  {
    amount: uint,
    yield-index: uint,
  }
)

;; Map of user principal to amount of STX they have borrowed
(define-map borrows
  { user: principal }
  {
    amount: uint,
    last-accrued: uint,
  }
)

;; ------------------------------------
;;               LENDING
;; ------------------------------------

;; @desc Deposits STX into the lending pool
;; @param amount: The amount of STX to deposit
;; @returns (response bool)
(define-public (deposit-stx (amount uint))
  (let (
      (user-deposit (map-get? deposits { user: tx-sender }))
      (deposited-stx (default-to u0 (get amount user-deposit)))
    )
    (asserts! (is-eq deposited-stx u0) ERR_MUST_WITHDRAW_BEFORE_NEW_DEPOSIT)
    (unwrap-panic (accrue-interest))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set deposits { user: tx-sender } {
      amount: (+ deposited-stx amount),
      yield-index: (var-get cumulative-yield-bips),
    })
    (var-set total-stx-deposits (+ (var-get total-stx-deposits) amount))
    (ok true)
  )
)

;; @desc Withdraws STX from the lending pool
;; @param amount: The amount of STX to withdraw
;; @returns (response bool)
(define-public (withdraw-stx (amount uint))
  (let (
      (user tx-sender)
      (user-deposit (map-get? deposits { user: user }))
      (deposited-stx (default-to u0 (get amount user-deposit)))
      (yield-index (default-to u0 (get yield-index user-deposit)))
      (pending-yield (unwrap-panic (get-pending-yield)))
    )
    (asserts! (>= deposited-stx amount) ERR_INVALID_WITHDRAW_AMOUNT)
    (unwrap-panic (accrue-interest))

    (map-set deposits { user: user } {
      amount: (- deposited-stx amount),
      yield-index: (var-get cumulative-yield-bips),
    })
    (var-set total-stx-deposits (- (var-get total-stx-deposits) amount))
    (try! (as-contract (stx-transfer? (+ amount pending-yield) tx-sender user)))
    (ok true)
  )
)

;; @desc Gets the total amount of pending STX yield that the lender has earned
;; The user is the tx-sender 
(define-read-only (get-pending-yield)
  (let (
      (user-deposit (map-get? deposits { user: tx-sender }))
      (yield-index (default-to u0 (get yield-index user-deposit)))
      (amount-stx (default-to u0 (get amount user-deposit)))
      (delta (- (var-get cumulative-yield-bips) yield-index))
      (pending-yield (/ (* amount-stx delta) u10000))
    )
    (ok pending-yield)
  )
)

;; ------------------------------------
;;               BORROWING
;; ------------------------------------

;; @desc Borrows STX from the lending pool
;; @param collateral-amount: The amount of sBTC to use as collateral
;; @param amount-stx: The amount of STX to borrow
;; @returns (response bool)
(define-public (borrow-stx
    (collateral-amount uint)
    (amount-stx uint)
  )
  (let (
      (user tx-sender)
      (user-collateral (map-get? collateral { user: user }))
      (deposited-sbtc (default-to u0 (get amount user-collateral)))
      (new-collateral (+ deposited-sbtc collateral-amount))
      (price (unwrap-panic (get-sbtc-stx-price)))
      (max-borrow (/ (* (* new-collateral price) LTV_PERCENTAGE) u100))
      (user-borrow (map-get? borrows { user: user }))
      (borrowed-stx (default-to u0 (get amount user-borrow)))
      (user-debt (unwrap-panic (get-debt user)))
      (new-debt (+ user-debt amount-stx))
    )
    (asserts! (<= new-debt max-borrow) ERR_EXCEEDED_MAX_BORROW)
    (unwrap-panic (accrue-interest))

    (map-set borrows { user: user } {
      amount: new-debt,
      last-accrued: (get-latest-timestamp),
    })
    (var-set total-stx-borrows (+ (var-get total-stx-borrows) amount-stx))

    (map-set collateral { user: user } { amount: new-collateral })
    (var-set total-sbtc-collateral
      (+ (var-get total-sbtc-collateral) collateral-amount)
    )

    (try! (contract-call? 'SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token
      transfer collateral-amount tx-sender (as-contract tx-sender) none
    ))
    (try! (as-contract (stx-transfer? amount-stx tx-sender user)))
    (ok true)
  )
)