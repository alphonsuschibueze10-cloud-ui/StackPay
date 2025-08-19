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
