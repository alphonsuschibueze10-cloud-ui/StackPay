;; title: StackPay - On-chain Invoicing & Payroll
;; version: 1.0.0
;; summary: Unified invoicing, payroll, and tax-friendly reporting for crypto-native businesses
;; description: Issue invoices, manage payroll, and settle in STX with BTC indexing

;; traits
(define-trait payment-trait
  ((pay (uint principal) (response bool uint))))

;; token definitions
;; Using native STX for settlements

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-already-paid (err u104))
(define-constant err-invalid-status (err u105))
(define-constant err-insufficient-balance (err u106))
(define-constant err-invalid-role (err u107))
(define-constant err-invalid-cycle (err u108))

;; Role constants
(define-constant role-admin u1)
(define-constant role-manager u2)
(define-constant role-employee u3)

;; Status constants
(define-constant status-pending u1)
(define-constant status-approved u2)
(define-constant status-paid u3)
(define-constant status-cancelled u4)

;; data vars
(define-data-var next-invoice-id uint u1)
(define-data-var next-payroll-id uint u1)
(define-data-var btc-stx-rate uint u50000) ;; BTC to STX rate (in micro-STX per satoshi)
(define-data-var platform-fee-rate uint u250) ;; 2.5% in basis points

;; data maps
;; User roles and permissions
(define-map user-roles principal uint)
(define-map user-permissions principal {can-create-invoice: bool, can-approve: bool, can-manage-payroll: bool})

;; Invoice data structure
(define-map invoices uint {
  issuer: principal,
  recipient: principal,
  amount-btc: uint, ;; Amount in satoshis
  amount-stx: uint, ;; Calculated STX amount
  description: (string-ascii 256),
  due-date: uint,
  status: uint,
  created-at: uint,
  paid-at: (optional uint),
  tx-hash: (optional (buff 32))
})

;; Payroll data structure
(define-map payroll-cycles uint {
  creator: principal,
  cycle-name: (string-ascii 64),
  start-date: uint,
  end-date: uint,
  total-amount-btc: uint,
  total-amount-stx: uint,
  status: uint,
  created-at: uint,
  processed-at: (optional uint)
})

;; Employee payroll entries
(define-map payroll-entries {cycle-id: uint, employee: principal} {
  amount-btc: uint,
  amount-stx: uint,
  status: uint,
  paid-at: (optional uint),
  tx-hash: (optional (buff 32))
})

;; Payment receipts for tax reporting
(define-map payment-receipts uint {
  payer: principal,
  recipient: principal,
  amount-stx: uint,
  amount-btc: uint,
  payment-type: (string-ascii 32), ;; "invoice" or "payroll"
  reference-id: uint,
  timestamp: uint,
  block-height: uint
})

(define-data-var next-receipt-id uint u1)

;; Company settings
(define-map company-settings principal {
  name: (string-ascii 128),
  tax-id: (string-ascii 64),
  auto-approve-threshold: uint, ;; Auto-approve invoices below this amount
  payroll-frequency: uint ;; Days between payroll cycles
})


;; public functions

;; Initialize user with role and permissions
(define-public (initialize-user (user principal) (role uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= role role-employee) err-invalid-role)
    (map-set user-roles user role)
    (map-set user-permissions user 
      {
        can-create-invoice: (or (is-eq role role-admin) (is-eq role role-manager)),
        can-approve: (or (is-eq role role-admin) (is-eq role role-manager)),
        can-manage-payroll: (or (is-eq role role-admin) (is-eq role role-manager))
      })
    (ok true)))

;; Create new invoice
(define-public (create-invoice (recipient principal) (amount-btc uint) (description (string-ascii 256)) (due-date uint))
  (let ((invoice-id (var-get next-invoice-id))
        (permissions (unwrap! (map-get? user-permissions tx-sender) err-unauthorized))
        (amount-stx (/ (* amount-btc (var-get btc-stx-rate)) u100000000))) ;; Convert satoshis to STX
    (asserts! (get can-create-invoice permissions) err-unauthorized)
    (asserts! (> amount-btc u0) err-invalid-amount)
    (map-set invoices invoice-id {
      issuer: tx-sender,
      recipient: recipient,
      amount-btc: amount-btc,
      amount-stx: amount-stx,
      description: description,
      due-date: due-date,
      status: status-pending,
      created-at: stacks-block-height,
      paid-at: none,
      tx-hash: none
    })
    (var-set next-invoice-id (+ invoice-id u1))
    (print {event: "invoice-created", invoice-id: invoice-id, recipient: recipient, amount-btc: amount-btc})
    (ok invoice-id)))

;; Approve invoice
(define-public (approve-invoice (invoice-id uint))
  (let ((invoice (unwrap! (map-get? invoices invoice-id) err-not-found))
        (permissions (unwrap! (map-get? user-permissions tx-sender) err-unauthorized)))
    (asserts! (get can-approve permissions) err-unauthorized)
    (asserts! (is-eq (get status invoice) status-pending) err-invalid-status)
    (map-set invoices invoice-id (merge invoice {status: status-approved}))
    (print {event: "invoice-approved", invoice-id: invoice-id})
    (ok true)))

;; Pay invoice
(define-public (pay-invoice (invoice-id uint))
  (let ((invoice (unwrap! (map-get? invoices invoice-id) err-not-found))
        (amount (get amount-stx invoice))
        (platform-fee (/ (* amount (var-get platform-fee-rate)) u10000))
        (net-amount (- amount platform-fee))
        (receipt-id (var-get next-receipt-id)))
    (asserts! (is-eq (get status invoice) status-approved) err-invalid-status)
    (asserts! (is-eq tx-sender (get recipient invoice)) err-unauthorized)
    
    ;; Transfer STX payment
    (try! (stx-transfer? net-amount tx-sender (get issuer invoice)))
    (try! (stx-transfer? platform-fee tx-sender contract-owner))
    
    ;; Update invoice status
    (map-set invoices invoice-id (merge invoice {
      status: status-paid,
      paid-at: (some stacks-block-height),
      tx-hash: none
    }))
    
    ;; Create payment receipt
    (map-set payment-receipts receipt-id {
      payer: tx-sender,
      recipient: (get issuer invoice),
      amount-stx: net-amount,
      amount-btc: (get amount-btc invoice),
      payment-type: "invoice",
      reference-id: invoice-id,
      timestamp: stacks-block-height,
      block-height: stacks-block-height
    })
    (var-set next-receipt-id (+ receipt-id u1))
    
    (print {event: "invoice-paid", invoice-id: invoice-id, amount: net-amount, receipt-id: receipt-id})
    (ok receipt-id)))

;; Create payroll cycle
(define-public (create-payroll-cycle (cycle-name (string-ascii 64)) (start-date uint) (end-date uint))
  (let ((cycle-id (var-get next-payroll-id))
        (permissions (unwrap! (map-get? user-permissions tx-sender) err-unauthorized)))
    (asserts! (get can-manage-payroll permissions) err-unauthorized)
    (asserts! (> end-date start-date) err-invalid-cycle)
    
    (map-set payroll-cycles cycle-id {
      creator: tx-sender,
      cycle-name: cycle-name,
      start-date: start-date,
      end-date: end-date,
      total-amount-btc: u0,
      total-amount-stx: u0,
      status: status-pending,
      created-at: stacks-block-height,
      processed-at: none
    })
    (var-set next-payroll-id (+ cycle-id u1))
    (print {event: "payroll-cycle-created", cycle-id: cycle-id, cycle-name: cycle-name})
    (ok cycle-id)))

;; Add employee to payroll cycle
(define-public (add-payroll-entry (cycle-id uint) (employee principal) (amount-btc uint))
  (let ((cycle (unwrap! (map-get? payroll-cycles cycle-id) err-not-found))
        (permissions (unwrap! (map-get? user-permissions tx-sender) err-unauthorized))
        (amount-stx (/ (* amount-btc (var-get btc-stx-rate)) u100000000)))
    (asserts! (get can-manage-payroll permissions) err-unauthorized)
    (asserts! (is-eq (get status cycle) status-pending) err-invalid-status)
    (asserts! (> amount-btc u0) err-invalid-amount)
    
    (map-set payroll-entries {cycle-id: cycle-id, employee: employee} {
      amount-btc: amount-btc,
      amount-stx: amount-stx,
      status: status-pending,
      paid-at: none,
      tx-hash: none
    })
    
    ;; Update cycle totals
    (map-set payroll-cycles cycle-id (merge cycle {
      total-amount-btc: (+ (get total-amount-btc cycle) amount-btc),
      total-amount-stx: (+ (get total-amount-stx cycle) amount-stx)
    }))
    
    (print {event: "payroll-entry-added", cycle-id: cycle-id, employee: employee, amount-btc: amount-btc})
    (ok true)))

;; Process payroll cycle
(define-public (process-payroll (cycle-id uint))
  (let ((cycle (unwrap! (map-get? payroll-cycles cycle-id) err-not-found))
        (permissions (unwrap! (map-get? user-permissions tx-sender) err-unauthorized)))
    (asserts! (get can-manage-payroll permissions) err-unauthorized)
    (asserts! (is-eq (get status cycle) status-pending) err-invalid-status)
    
    (map-set payroll-cycles cycle-id (merge cycle {
      status: status-approved,
      processed-at: (some stacks-block-height)
    }))
    
    (print {event: "payroll-processed", cycle-id: cycle-id})
    (ok true)))

;; Pay individual payroll entry
(define-public (pay-payroll-entry (cycle-id uint) (employee principal))
  (let ((entry (unwrap! (map-get? payroll-entries {cycle-id: cycle-id, employee: employee}) err-not-found))
        (cycle (unwrap! (map-get? payroll-cycles cycle-id) err-not-found))
        (amount (get amount-stx entry))
        (platform-fee (/ (* amount (var-get platform-fee-rate)) u10000))
        (net-amount (- amount platform-fee))
        (receipt-id (var-get next-receipt-id)))
    (asserts! (is-eq (get status cycle) status-approved) err-invalid-status)
    (asserts! (is-eq (get status entry) status-pending) err-invalid-status)
    
    ;; Transfer STX payment
    (try! (stx-transfer? net-amount tx-sender employee))
    (try! (stx-transfer? platform-fee tx-sender contract-owner))
    
    ;; Update entry status
    (map-set payroll-entries {cycle-id: cycle-id, employee: employee} (merge entry {
      status: status-paid,
      paid-at: (some stacks-block-height),
      tx-hash: none
    }))
    
    ;; Create payment receipt
    (map-set payment-receipts receipt-id {
      payer: tx-sender,
      recipient: employee,
      amount-stx: net-amount,
      amount-btc: (get amount-btc entry),
      payment-type: "payroll",
      reference-id: cycle-id,
      timestamp: stacks-block-height,
      block-height: stacks-block-height
    })
    (var-set next-receipt-id (+ receipt-id u1))
    
    (print {event: "payroll-paid", cycle-id: cycle-id, employee: employee, amount: net-amount, receipt-id: receipt-id})
    (ok receipt-id)))

;; Update BTC/STX exchange rate
(define-public (update-btc-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set btc-stx-rate new-rate)
    (print {event: "btc-rate-updated", new-rate: new-rate})
    (ok true)))

;; read only functions

;; Get invoice details
(define-read-only (get-invoice (invoice-id uint))
  (map-get? invoices invoice-id))

;; Get payroll cycle details
(define-read-only (get-payroll-cycle (cycle-id uint))
  (map-get? payroll-cycles cycle-id))

;; Get payroll entry details
(define-read-only (get-payroll-entry (cycle-id uint) (employee principal))
  (map-get? payroll-entries {cycle-id: cycle-id, employee: employee}))

;; Get payment receipt
(define-read-only (get-payment-receipt (receipt-id uint))
  (map-get? payment-receipts receipt-id))

;; Get user role
(define-read-only (get-user-role (user principal))
  (map-get? user-roles user))

;; Get user permissions
(define-read-only (get-user-permissions (user principal))
  (map-get? user-permissions user))

;; Get current BTC rate
(define-read-only (get-btc-rate)
  (var-get btc-stx-rate))

;; Convert BTC amount to STX
(define-read-only (btc-to-stx (amount-btc uint))
  (/ (* amount-btc (var-get btc-stx-rate)) u100000000))

;; Get platform fee for amount
(define-read-only (get-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-rate)) u10000))

;; Check if user can create invoices
(define-read-only (can-create-invoice (user principal))
  (match (map-get? user-permissions user)
    permissions (get can-create-invoice permissions)
    false))

;; Check if user can approve
(define-read-only (can-approve (user principal))
  (match (map-get? user-permissions user)
    permissions (get can-approve permissions)
    false))

;; Check if user can manage payroll
(define-read-only (can-manage-payroll (user principal))
  (match (map-get? user-permissions user)
    permissions (get can-manage-payroll permissions)
    false))

;; Get total invoices created
(define-read-only (get-total-invoices)
  (- (var-get next-invoice-id) u1))

;; Get total payroll cycles created
(define-read-only (get-total-payroll-cycles)
  (- (var-get next-payroll-id) u1))

;; Get total receipts created
(define-read-only (get-total-receipts)
  (- (var-get next-receipt-id) u1))

;; private functions

;; Calculate net amount after platform fee
(define-private (calculate-net-amount (gross-amount uint))
  (let ((fee (/ (* gross-amount (var-get platform-fee-rate)) u10000)))
    (- gross-amount fee)))

;; Validate user permissions for action
(define-private (validate-permissions (user principal) (required-permission (string-ascii 32)))
  (let ((permissions (default-to {can-create-invoice: false, can-approve: false, can-manage-payroll: false}
                                 (map-get? user-permissions user))))
    (if (is-eq required-permission "create-invoice")
        (get can-create-invoice permissions)
        (if (is-eq required-permission "approve")
            (get can-approve permissions)
            (if (is-eq required-permission "manage-payroll")
                (get can-manage-payroll permissions)
                false)))))
