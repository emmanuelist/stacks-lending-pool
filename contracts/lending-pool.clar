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