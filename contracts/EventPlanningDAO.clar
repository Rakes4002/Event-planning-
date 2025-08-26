;; EventPlanning DAO Smart Contract
;; A decentralized autonomous organization for community-driven event planning
;; with funding mechanisms and profit sharing capabilities

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-insufficient-funds (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-event-not-found (err u104))
(define-constant err-event-already-exists (err u105))
(define-constant err-voting-closed (err u106))

;; Data Variables
(define-data-var next-event-id uint u1)
(define-data-var dao-treasury uint u0)
(define-data-var total-members uint u0)

;; Event Structure
(define-map events 
  uint 
  {
    title: (string-ascii 64),
    description: (string-ascii 256),
    organizer: principal,
    funding-goal: uint,
    current-funding: uint,
    profit-share-percentage: uint,
    status: (string-ascii 20),
    created-at: uint,
    contributors-count: uint
  })

;; Event Contributors (tracks who contributed and how much)
(define-map event-contributors 
  {event-id: uint, contributor: principal}
  {amount: uint, profit-share: uint})

;; DAO Members
(define-map dao-members principal uint) ;; member -> joining block

;; Member voting power (based on total contributions)
(define-map member-voting-power principal uint)

;; Function 1: Create Event Proposal
;; Allows DAO members to propose new events with funding goals
(define-public (create-event-proposal 
  (title (string-ascii 64))
  (description (string-ascii 256))
  (funding-goal uint)
  (profit-share-percentage uint))
  (let ((event-id (var-get next-event-id)))
    (begin
      ;; Validate inputs
      (asserts! (> funding-goal u0) err-invalid-amount)
      (asserts! (<= profit-share-percentage u100) err-invalid-amount)
      (asserts! (> (len title) u0) err-invalid-amount)
      
      ;; Check if member exists, if not add them
      (if (is-none (map-get? dao-members tx-sender))
        (begin
          (map-set dao-members tx-sender stacks-block-height)
          (var-set total-members (+ (var-get total-members) u1)))
        true)
      
      ;; Create the event
      (map-set events event-id {
        title: title,
        description: description,
        organizer: tx-sender,
        funding-goal: funding-goal,
        current-funding: u0,
        profit-share-percentage: profit-share-percentage,
        status: "funding",
        created-at: stacks-block-height,
        contributors-count: u0
      })
      
      ;; Increment event ID for next event
      (var-set next-event-id (+ event-id u1))
      
      ;; Print event creation log
      (print {
        action: "event-created",
        event-id: event-id,
        organizer: tx-sender,
        funding-goal: funding-goal
      })
      
      (ok event-id))))

;; Function 2: Fund Event and Earn Profit Share
;; Allows community members to fund events and earn profit sharing rights
(define-public (fund-event (event-id uint) (amount uint))
  (let ((event-data (unwrap! (map-get? events event-id) err-event-not-found))
        (existing-contribution (default-to u0 
          (get amount (map-get? event-contributors {event-id: event-id, contributor: tx-sender})))))
    (begin
      ;; Validate funding amount
      (asserts! (> amount u0) err-invalid-amount)
      
      ;; Check if event is still in funding status
      (asserts! (is-eq (get status event-data) "funding") err-voting-closed)
      
      ;; Transfer STX from contributor to contract
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      
      ;; Update DAO treasury
      (var-set dao-treasury (+ (var-get dao-treasury) amount))
      
      ;; Calculate profit share based on contribution percentage
      (let ((new-total-funding (+ (get current-funding event-data) amount))
            (contribution-percentage (/ (* amount u10000) (get funding-goal event-data)))
            (profit-share (/ (* contribution-percentage (get profit-share-percentage event-data)) u10000)))
        
        ;; Update event contributors
        (map-set event-contributors 
          {event-id: event-id, contributor: tx-sender}
          {
            amount: (+ existing-contribution amount),
            profit-share: profit-share
          })
        
        ;; Update event funding status
        (map-set events event-id (merge event-data {
          current-funding: new-total-funding,
          contributors-count: (if (is-eq existing-contribution u0) 
                                (+ (get contributors-count event-data) u1)
                                (get contributors-count event-data))
        }))
        
        ;; Add contributor to DAO members if not already a member
        (if (is-none (map-get? dao-members tx-sender))
          (begin
            (map-set dao-members tx-sender stacks-block-height)
            (var-set total-members (+ (var-get total-members) u1)))
          true)
        
        ;; Update member voting power
        (map-set member-voting-power tx-sender 
          (+ (default-to u0 (map-get? member-voting-power tx-sender)) amount))
        
        ;; Check if funding goal is reached
        (if (>= new-total-funding (get funding-goal event-data))
          (map-set events event-id (merge event-data {
            current-funding: new-total-funding,
            status: "funded"
          }))
          true)
        
        ;; Print funding log
        (print {
          action: "event-funded",
          event-id: event-id,
          contributor: tx-sender,
          amount: amount,
          profit-share: profit-share,
          total-funding: new-total-funding
        })
        
        (ok {
          contribution-amount: amount,
          profit-share-percentage: profit-share,
          total-event-funding: new-total-funding,
          funding-goal-reached: (>= new-total-funding (get funding-goal event-data))
        })))))

;; Read-only functions for querying contract state

;; Get event details
(define-read-only (get-event (event-id uint))
  (map-get? events event-id))

;; Get contributor details for specific event
(define-read-only (get-contributor-details (event-id uint) (contributor principal))
  (map-get? event-contributors {event-id: event-id, contributor: contributor}))

;; Get DAO treasury balance
(define-read-only (get-dao-treasury)
  (var-get dao-treasury))

;; Get total DAO members
(define-read-only (get-total-members)
  (var-get total-members))

;; Get member voting power
(define-read-only (get-voting-power (member principal))
  (default-to u0 (map-get? member-voting-power member)))

;; Get next event ID
(define-read-only (get-next-event-id)
  (var-get next-event-id))






